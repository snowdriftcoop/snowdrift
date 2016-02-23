{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TupleSections #-}

module Application
    ( makeApplication
    , getApplicationDev
    , makeFoundation
    ) where

import Import

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (atomically, newTChanIO, tryReadTChan)
import Control.Monad.Logger (runLoggingT, runStderrLoggingT)
import Control.Monad.Reader
import Control.Monad.Trans.Resource
import Data.Default (def)
import Database.Persist.Postgresql (pgConnStr, withPostgresqlConn)
import Network.HTTP.Client.Conduit (newManager)
import Network.Wai.Logger (clockDateCacher)
import Network.Wai.Middleware.RequestLogger
            (mkRequestLogger
            ,outputFormat
            ,OutputFormat (..)
            ,IPAddrSource (..)
            ,destination)
import System.Environment
import System.Log.FastLogger (newStdoutLoggerSet, defaultBufSize)
import Yesod.Core.Types (loggerSet, Logger (Logger))
import Yesod.Default.Config
import Yesod.Default.Main hiding (LogFunc)
import qualified Data.List as L
import qualified Database.Persist
import qualified Network.Wai.Middleware.RequestLogger as RequestLogger

import SnowdriftEventHandler
import Version
import Migrations
import Widgets.Navbar

-- Handlers!
import Handler.BuildFeed
import Handler.Comment
import Handler.Common
import Handler.HonorPledge
import Handler.Image
import Handler.Invitation
import Handler.JsLicenses
import Handler.MarkdownTutorial
import Handler.NewDesign
import Handler.Notification
import Handler.PostLogin
import Handler.Project
import Handler.ProjectBlog
import Handler.ResetPassphrase
import Handler.Simple
import Handler.SnowdriftEvent
import Handler.User
import Handler.Volunteer
import Handler.Who
import Handler.Widget
import Handler.Wiki
import Handler.Wiki.Comment

runSql :: MonadIO m => Text -> ReaderT SqlBackend m ()
runSql = flip rawExecute [] -- TODO quasiquoter?

version :: (Text, Text)
version = $(mkVersion)

-- This line actually creates our YesodDispatch instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see the
-- comments there for more details.
mkYesodDispatch "App" resourcesApp

-- probably not thread safe
withEnv :: (MonadIO m) => String -> String -> m a -> m a
withEnv k v action = do
    original <- liftIO $ lookupEnv k

    liftIO $ setEnv k v
    result <- action
    liftIO $ maybe (unsetEnv k) (setEnv k) original

    return result

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeApplication :: AppConfig DefaultEnv Extra -> IO (Application, LogFunc)
makeApplication conf = do
    foundation <- makeFoundation conf

    -- Initialize the logging middleware
    logWare <- mkRequestLogger def
        { outputFormat =
            if development
                then Detailed True
                else Apache FromSocket
        , destination = RequestLogger.Logger $ loggerSet $ appLogger foundation
        }

    -- Create the WAI application and apply middlewares
    app <- toWaiAppPlain foundation
    let logFunc = messageLoggerSource foundation (appLogger foundation)
    return (logWare $ defaultMiddlewaresNoLogging app, logFunc)

-- | Loads up any necessary settings, creates your foundation datatype, and
-- performs some initialization.
makeFoundation :: AppConfig DefaultEnv Extra -> IO App
makeFoundation conf = do
    manager <- newManager
    s <- staticSite
    dbconf <- withYamlEnvironment "config/postgresql.yml" (appEnv conf)
              Database.Persist.loadConfig >>=
              Database.Persist.applyEnv
    pool <- Database.Persist.createPoolConfig dbconf

    loggerSet' <- newStdoutLoggerSet defaultBufSize
    (getter, _) <- clockDateCacher

    event_chan <- newTChanIO
    let logger = Yesod.Core.Types.Logger loggerSet' getter
        foundation = App
                     { appNavbar = navbar
                     , appSettings = conf
                     , appStatic = s
                     , appConnPool = pool
                     , appHttpManager = manager
                     , persistConfig = dbconf
                     , appLogger = logger
                     , appEventChan = event_chan
                     , appEventHandlers = snowdriftEventHandlers
                     }

    -- Database setup
    case appEnv conf of
        -- Drop and recreate the database when testing.
        Testing -> withEnv "PGDATABASE" "template1"
            (applyEnv $ persistConfig foundation) >>= \dbconf' -> do
                options <- maybe [] L.words <$> lookupEnv
                                                    "SNOWDRIFT_TESTING_OPTIONS"

                unless
                    (elem "nodrop" options)
                    (runStderrLoggingT $
                        runResourceT $
                            withPostgresqlConn (pgConnStr dbconf') $
                                runReaderT $ do
                                    runSql "DROP DATABASE IF EXISTS snowdrift_test;"
                                    runSql $ "CREATE DATABASE snowdrift_test "
                                             <> "WITH TEMPLATE snowdrift_test_template;")
        -- Keep a record of deployed versions.
        Production -> do
            now <- getCurrentTime
            let (base, diff) = version
            runLoggingT
                (Database.Persist.runPool
                    dbconf
                    (insert_ $ Build now base diff)
                    pool)
                (messageLoggerSource foundation logger)
        _ -> return ()

    -- Perform database migration using our application's logging settings.
    let migration = runSqlPool
            (doManualMigration >> runMigration migrateAll >> migrateTriggers)
            pool
    void $ runLoggingT
        migration
        (messageLoggerSource foundation logger)

    forkEventHandler foundation

    return foundation

-- for yesod devel
getApplicationDev :: IO (Int, Application)
getApplicationDev =
    defaultDevelApp loader (fmap fst . makeApplication)
  where
    loader = Yesod.Default.Config.loadConfig (configSettings Development)
        { csParseExtra = parseExtra
        }

--------------------------------------------------------------------------------
-- SnowdriftEvent handling

forkEventHandler :: App -> IO ()
forkEventHandler app@App{..} = void . forkIO . forever $ do
    threadDelay 1000000 -- Sleep for one second in between runs.
    handleNEvents 10     -- Handle up to 10 events per run.
  where
    handleNEvents :: Int -> IO ()
    handleNEvents 0 = return ()
    handleNEvents n = atomically (tryReadTChan appEventChan) >>= \case
        Nothing    -> return ()
        Just event -> do
            mapM_ (runDaemon app) (appEventHandlers appSettings <*> [event])
            handleNEvents (n-1)
