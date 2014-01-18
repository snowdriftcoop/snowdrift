{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
    ( makeApplication
    , getApplicationDev
    , makeFoundation
    ) where

import Import
import Settings
import Yesod.Default.Config
import Yesod.Default.Main
import Yesod.Default.Handlers
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import qualified Database.Persist
import qualified Database.Persist.Sql
import Network.HTTP.Conduit (newManager, def)
import Version
import Control.Monad.Logger (runLoggingT)
import Control.Monad.Trans.Resource
import System.IO (stdout)
import System.Log.FastLogger (mkLogger)

import Data.Text as T

-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!
import Handler.Home
import Handler.Faq
import Handler.User
import Handler.Widget
import Handler.Project
import Handler.Invitation
import Handler.Invite
import Handler.UpdateShares
import Handler.Volunteer
import Handler.Contact
import Handler.Who
import Handler.PostLogin
import Handler.ToU
import Handler.Privacy
import Handler.Messages
import Handler.Application
import Handler.Applications
import Handler.JsLicense
import Handler.MarkdownTutorial
import Handler.UserBalance
import Handler.UserPledges
import Handler.Wiki
import Handler.Tags
import Handler.Tickets
import Handler.RepoFeed
import Handler.BuildFeed

import Widgets.Navbar

version :: (Text, Text)
version = $(mkVersion)

-- This line actually creates our YesodDispatch instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see the
-- comments there for more details.
mkYesodDispatch "App" resourcesApp

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeApplication :: AppConfig DefaultEnv Extra -> IO Application
makeApplication conf = do
    foundation <- makeFoundation conf
    app <- toWaiAppPlain foundation
    return $ logWare app
  where
    logWare   = if development then logStdoutDev
                               else logStdout

makeFoundation :: AppConfig DefaultEnv Extra -> IO App
makeFoundation conf = do
    manager <- newManager def
    s <- staticSite
    dbconf <- withYamlEnvironment "config/postgresql.yml" (appEnv conf)
              Database.Persist.loadConfig >>=
              Database.Persist.applyEnv
    p <- Database.Persist.createPoolConfig (dbconf :: Settings.PersistConf)
    logger <- mkLogger True stdout
    let foundation = App navbar conf s p manager dbconf logger

    flip runLoggingT (messageLoggerSource foundation logger) $ do
        Database.Persist.runPool dbconf (printMigration migrateAll >> runMigration migrateAll >> rolloutStagingWikiPages) p
        Database.Persist.Sql.runSqlPool migrateTriggers p

    now <- getCurrentTime
    let (base, diff) = version
    runLoggingT
        (runResourceT $ Database.Persist.runPool dbconf (insert_ $ Build now base diff) p)
        (messageLoggerSource foundation logger)

    return foundation

-- for yesod devel
getApplicationDev :: IO (Int, Application)
getApplicationDev =
    defaultDevelApp loader makeApplication
  where
    loader = Yesod.Default.Config.loadConfig (configSettings Development)
        { csParseExtra = parseExtra
        }

rolloutStagingWikiPages :: (MonadBaseControl IO m, MonadIO m, MonadLogger m, MonadUnsafeIO m, MonadThrow m) => SqlPersistT m ()
rolloutStagingWikiPages = do
    pages <- select $ from $ \ page -> do
        where_ ( page ^. WikiPageTarget `like` val "_staging_%" )
        return page

    forM_ pages $ \ (Entity staged_page_id staged_page) -> do
        let (Just target) = stripPrefix "_staging_" $ wikiPageTarget staged_page
        [ Value page_id ] <- select $ from $ \ page -> do
            where_ ( page ^. WikiPageTarget ==. val target )
            return $ page ^. WikiPageId

        update $ \ edit -> do
            set edit [ WikiEditPage =. val page_id ]
            where_ ( edit ^. WikiEditPage ==. val staged_page_id )
    
        update $ \ page -> do
            set page [ WikiPageContent =. val (wikiPageContent staged_page) ]
            where_ ( page ^. WikiPageId ==. val page_id )

        [ Value last_staged_edit_edit ] <- select $ from $ \ last_staged_edit -> do
            where_ ( last_staged_edit ^. WikiLastEditPage ==. val staged_page_id )
            return $ last_staged_edit ^. WikiLastEditEdit
                    
        update $ \ last_edit -> do
            set last_edit [ WikiLastEditEdit =. val last_staged_edit_edit ]
            where_ ( last_edit ^. WikiLastEditPage ==. val page_id )

        delete $ from $ \ last_edit -> do
            where_ ( last_edit ^. WikiLastEditPage ==. val staged_page_id )

        delete $ from $ \ page -> do
            where_ ( page ^. WikiPageId ==. val staged_page_id )


migrateTriggers :: (MonadSqlPersist m, MonadBaseControl IO m, MonadUnsafeIO m, MonadThrow m) => m ()
migrateTriggers = runResourceT $ do
    flip rawExecute [] $ T.unlines
        [ "CREATE OR REPLACE FUNCTION log_role_event_trigger() RETURNS trigger AS $role_event$"
        , "    BEGIN"
        , "        IF (TG_OP = 'DELETE') THEN"
        , "            INSERT INTO role_event (time, \"user\", role, project, added) SELECT now(), OLD.\"user\", OLD.role, OLD.project, 'f';"
        , "            RETURN OLD;"
        , "        ELSIF (TG_OP = 'INSERT') THEN"
        , "            INSERT INTO role_event (time, \"user\", role, project, added) SELECT now(), NEW.\"user\", NEW.role, NEW.project, 't';"
        , "            RETURN NEW;"
        , "        END IF;"
        , "        RETURN NULL;"
        , "    END;"
        , "$role_event$ LANGUAGE plpgsql;"
        ]

    flip rawExecute [] "DROP TRIGGER IF EXISTS role_event ON project_user_role;"

    flip rawExecute [] $ T.unlines
        [ "CREATE TRIGGER role_event"
        , "AFTER INSERT OR DELETE ON project_user_role"
        , "    FOR EACH ROW EXECUTE PROCEDURE log_role_event_trigger();"
        ]

    flip rawExecute [] $ T.unlines
        [ "CREATE OR REPLACE FUNCTION log_doc_event_trigger() RETURNS trigger AS $doc_event$"
        , "    BEGIN"
        , "        IF (TG_OP = 'INSERT' OR TG_OP = 'UPDATE') THEN"
        , "            INSERT INTO doc_event (time, doc, blessed_version) SELECT now(), NEW.id, NEW.current_version;"
        , "            RETURN NEW;"
        , "        END IF;"
        , "        RETURN NULL;"
        , "    END;"
        , "$doc_event$ LANGUAGE plpgsql;"
        ]

    flip rawExecute [] "DROP TRIGGER IF EXISTS doc_event ON doc;"

    flip rawExecute [] $ T.unlines
        [ "CREATE TRIGGER doc_event"
        , "AFTER INSERT OR DELETE ON doc"
        , "    FOR EACH ROW EXECUTE PROCEDURE log_doc_event_trigger();"
        ]

    return ()
        
