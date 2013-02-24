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
import qualified Database.Persist.Store
import Database.Persist.GenericSql (printMigration, runMigration)
import Network.HTTP.Conduit (newManager, def)

import Control.Monad.Logger

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
import Handler.Committee
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
    runStderrLoggingT $ Database.Persist.Store.runPool dbconf (printMigration migrateAll >> runMigration migrateAll) p
    return $ App conf s p manager dbconf

-- for yesod devel
getApplicationDev :: IO (Int, Application)
getApplicationDev =
    defaultDevelApp loader makeApplication
  where
    loader = loadConfig (configSettings Development)
        { csParseExtra = parseExtra
        }
