{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TupleSections #-}

module Application
    ( makeApplication
    , getApplicationDev
    , makeFoundation
    ) where

import Import
import SnowdriftEventHandler
import Version

import           Control.Concurrent                   (forkIO, threadDelay)
import           Control.Concurrent.STM               (atomically, newTChanIO, tryReadTChan)
import           Control.Monad.Logger                 (runLoggingT, runStderrLoggingT)
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import           Data.Default                         (def)
import qualified Data.List                            as L
import           Data.Text                            as T
import qualified Data.Text.IO                         as T
import qualified Database.Persist
import           Database.Persist.Postgresql          (pgConnStr, withPostgresqlConn)
import           Network.HTTP.Client.Conduit          (newManager)
import           Network.Wai.Middleware.RequestLogger ( mkRequestLogger, outputFormat, OutputFormat (..)
                                                      , IPAddrSource (..), destination
                                                      )
import           Network.Wai.Logger                   (clockDateCacher)
import qualified Network.Wai.Middleware.RequestLogger as RequestLogger
import           System.Directory
import           System.Environment
import           System.Log.FastLogger                (newStdoutLoggerSet, defaultBufSize, flushLogStr)
import           System.IO                            (stderr)
import           Yesod.Core.Types                     (loggerSet, Logger (Logger))
import           Yesod.Default.Config
import           Yesod.Default.Handlers
import           Yesod.Default.Main hiding (LogFunc)

-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!

import Handler.BuildFeed
import Handler.Comment
import Handler.Home
import Handler.Donate
import Handler.HonorPledge
import Handler.Image
import Handler.Invitation
import Handler.JsLicense
import Handler.MarkdownTutorial
import Handler.Notification
import Handler.PostLogin
import Handler.Privacy
import Handler.Project
import Handler.Project.Signup
import Handler.ProjectBlog
import Handler.RepoFeed
import Handler.ResetPassword
import Handler.SnowdriftEvent
import Handler.ToU
import Handler.Trademarks
import Handler.User
import Handler.User.Comment
import Handler.Volunteer
import Handler.Who
import Handler.Widget
import Handler.Wiki
import Handler.Wiki.Comment

import Widgets.Navbar

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
    (getter, updater) <- clockDateCacher

    -- If the Yesod logger (as opposed to the request logger middleware) is
    -- used less than once a second on average, you may prefer to omit this
    -- thread and use "(updater >> getter)" in place of "getter" below.  That
    -- would update the cache every time it is used, instead of every second.
    let updateLoop = forever $ do
            threadDelay 1000000
            updater
            flushLogStr loggerSet'
            updateLoop
    void $ forkIO updateLoop

    event_chan <- newTChanIO
    let logger = Yesod.Core.Types.Logger loggerSet' getter
        foundation = App
                       navbar
                       conf
                       s
                       pool
                       manager
                       dbconf
                       logger
                       event_chan
                       snowdriftEventHandlers

    -- Perform database migration using our application's logging settings.
    case appEnv conf of
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
                                    liftIO $ putStrLn "dropping database..."
                                    runSql "DROP DATABASE IF EXISTS snowdrift_test;"
                                    liftIO $ putStrLn "creating database..."
                                    runSql $ "CREATE DATABASE snowdrift_test "
                                             <> "WITH TEMPLATE snowdrift_test_template;"
                                    liftIO $ putStrLn "ready.")
        _ -> return ()

    let migration = runSqlPool
            (doManualMigration >> runMigration migrateAll >> migrateTriggers)
            pool

    void $ runLoggingT
        migration
        (messageLoggerSource foundation logger)

    now <- getCurrentTime
    let (base, diff) = version
    runLoggingT
        (Database.Persist.runPool
            dbconf
            (insert_ $ Build now base diff)
            pool)
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

doManualMigration :: (MonadIO m, MonadLogger m, Functor m)
                  => SqlPersistT m ()
doManualMigration = do
    deprecatedApplyManualMigrations
    saveUnsafeMigrations

deprecatedApplyManualMigrations
    :: (MonadLogger m, MonadIO m)
    => ReaderT SqlBackend m ()
deprecatedApplyManualMigrations = do
    $(logInfo) "creating version table"

    runSql "CREATE TABLE IF NOT EXISTS \"database_version\" (\"id\" SERIAL PRIMARY KEY UNIQUE, \"last_migration\" INT8 NOT NULL);"

    last_migration <- select $ from return

    migration_number <- case last_migration of
        [] -> insert (DatabaseVersion 0) >> return 0
        [Entity _ (DatabaseVersion migration)] -> return migration
        _ -> error "multiple entries in DB version table"

    unfiltered_migration_files <- liftIO $ getDirectoryContents "migrations"

    let migration_files :: [(Int, String)]
        migration_files = L.sort
            $ L.filter ((> migration_number) . fst)
            $ mapMaybe (\s -> fmap (,s) $ readMaybe =<< L.stripPrefix "migrate" s)
            unfiltered_migration_files

    forM_ (L.map (("migrations/" <>) . snd) migration_files) $ \file -> do
        $(logWarn) $ "running " <> T.pack file <> "..."
        migration <- liftIO $ T.readFile file
        runSql migration

    let new_last_migration = L.maximum $ migration_number : L.map fst migration_files

    update $ flip set [ DatabaseVersionLastMigration =. val new_last_migration ]

    -- This is where new safe migrations used to be recorded.

-- | Automatically save unsafe migrations, if they exist.
--
-- We'll still blow up with runMigration, but we'll be one step closer to
-- storing the necessary sql statements to commit and share.
saveUnsafeMigrations :: (MonadIO m, Functor m) => ReaderT SqlBackend m ()
saveUnsafeMigrations = do
    unsafe <- L.filter fst <$> parseMigration' migrateAll
    unless (L.null $ L.map snd unsafe) $ do
        liftIO $ T.writeFile filename $ T.unlines $ L.map ((`snoc` ';') . snd) unsafe
        liftIO $ mapM_ (T.hPutStrLn stderr) unsafeMigMessages
  where
    filename = "migrations/migrate.unsafe"
    unsafeMigMessages =
      [ ""
      , "*** UNSAFE MIGRATIONS EXIST"
      , ""
      , "The application will now exit. But first, unsafe migrations have been"
      , "stored in «" <> filename <> "». Please review them, rename them"
      , "appropriately, and commit them with your change."
      , ""
      , "More information follows."
      , ""
      ]


migrateTriggers :: MonadIO m => ReaderT SqlBackend m ()
migrateTriggers = do
    runSql $ T.unlines
        [ "CREATE OR REPLACE FUNCTION log_role_event_trigger() RETURNS trigger AS $role_event$"
        , "    BEGIN"
        , "        IF (TG_OP = 'DELETE') THEN"
        , "            INSERT INTO role_event (ts, \"user\", role, project, added) SELECT now(), OLD.\"user\", OLD.role, OLD.project, 'f';"
        , "            RETURN OLD;"
        , "        ELSIF (TG_OP = 'INSERT') THEN"
        , "            INSERT INTO role_event (ts, \"user\", role, project, added) SELECT now(), NEW.\"user\", NEW.role, NEW.project, 't';"
        , "            RETURN NEW;"
        , "        END IF;"
        , "        RETURN NULL;"
        , "    END;"
        , "$role_event$ LANGUAGE plpgsql;"
        ]

    runSql "DROP TRIGGER IF EXISTS role_event ON project_user_role;"

    runSql $ T.unlines
        [ "CREATE TRIGGER role_event"
        , "AFTER INSERT OR DELETE ON project_user_role"
        , "    FOR EACH ROW EXECUTE PROCEDURE log_role_event_trigger();"
        ]

    runSql $ T.unlines
        [ "CREATE OR REPLACE FUNCTION log_doc_event_trigger() RETURNS trigger AS $doc_event$"
        , "    BEGIN"
        , "        IF (TG_OP = 'INSERT' OR TG_OP = 'UPDATE') THEN"
        , "            INSERT INTO doc_event (ts, doc, blessed_version) SELECT now(), NEW.id, NEW.current_version;"
        , "            RETURN NEW;"
        , "        END IF;"
        , "        RETURN NULL;"
        , "    END;"
        , "$doc_event$ LANGUAGE plpgsql;"
        ]

    runSql "DROP TRIGGER IF EXISTS doc_event ON doc;"

    runSql $ T.unlines
        [ "CREATE TRIGGER doc_event"
        , "AFTER INSERT OR DELETE ON doc"
        , "    FOR EACH ROW EXECUTE PROCEDURE log_doc_event_trigger();"
        ]

    return ()

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
            mapM_ (runDaemon app) (appEventHandlers settings <*> [event])
            handleNEvents (n-1)
