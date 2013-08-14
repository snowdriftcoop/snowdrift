{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
    ( makeApplication
    , getApplicationDev
    , makeFoundation
    ) where

import Import hiding ((==.), (=.), update, delete)
import Settings
import Yesod.Default.Config
import Yesod.Default.Main
import Yesod.Default.Handlers
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import qualified Database.Persist.Store
import Database.Esqueleto hiding (loadConfig)
import Network.HTTP.Conduit (newManager, def)
import Version
import Control.Monad.Trans.Resource
import Control.Monad.IO.Class
import Control.Monad.Logger

import Data.Text

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
import Handler.Tos
import Handler.Privacy
import Handler.Messages
import Handler.Application
import Handler.Applications
import Handler.JsLicense
import Handler.MarkdownTutorial
import Handler.UserBalance
import Handler.UserPledges
import Handler.Wiki
import Handler.Tickets
import Handler.RepoFeed
import Handler.BuildFeed
import Handler.ProjectApplication


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
              Database.Persist.Store.loadConfig >>=
              Database.Persist.Store.applyEnv
    p <- Database.Persist.Store.createPoolConfig (dbconf :: Settings.PersistConfig)
    runStderrLoggingT $ Database.Persist.Store.runPool dbconf (printMigration migrateAll >> runMigration migrateAll >> rolloutStagingWikiPages) p

    now <- getCurrentTime
    let (base, diff) = version
    runStderrLoggingT $ runResourceT $ Database.Persist.Store.runPool dbconf (insert_ $ Build now base diff) p

    return $ App conf s p manager dbconf

-- for yesod devel
getApplicationDev :: IO (Int, Application)
getApplicationDev =
    defaultDevelApp loader makeApplication
  where
    loader = loadConfig (configSettings Development)
        { csParseExtra = parseExtra
        }

rolloutStagingWikiPages :: (MonadBaseControl IO m, MonadIO m, MonadLogger m, MonadUnsafeIO m, MonadThrow m) => SqlPersist m ()
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

        
