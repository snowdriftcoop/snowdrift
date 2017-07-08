module TestImport
    ( module TestImport
    , module X
    ) where

import ClassyPrelude as X hiding (delete, deleteBy)
import Database.Persist as X hiding (get)
import Database.Persist.Sql (SqlPersistM, runSqlPersistMPool)
import Database.Persist.Postgresql (pgConnStr, ConnectionString)
import Foundation as X
import Network.HTTP.Types (Status(..))
import Network.Wai.Test (SResponse(..))
import Test.Tasty.Hspec as X
import Yesod.Default.Config2 (ignoreEnv, loadYamlSettings)
import Yesod.Test as X
import qualified Data.Text.Encoding as T
import qualified Database.PostgreSQL.Simple as PG

-- For htmlHasLink
import Test.Tasty.HUnit
import Yesod.Core

import Application (makeFoundation, makeLogWare)
import AppDataTypes as X
import AuthSite
import Model as X
import Settings (AppSettings(..))

import Factories

-- | Run a query outside of a handler
testDB :: SqlPersistM a -> YesodExample App a
testDB query = do
    app <- getTestYesod
    liftIO $ runDBWithApp app query

runDBWithApp :: App -> SqlPersistM a -> IO a
runDBWithApp app query = runSqlPersistMPool query (appConnPool app)

dummyLogin :: YesodExample App ()
dummyLogin = do
    testDB $ createUser "alice" "ccccccccccccc"
    get (AuthR LoginR)
    request $ do
        addToken
        byLabel "What is your email?" "alice"
        byLabel "Please tell us your passphrase, too." "ccccccccccccc"
        setMethod "POST"
        setUrl (AuthR LoginR)

login :: Text -> Text -> YesodExample App ()
login username password = do
    get (AuthR LoginR)

    request $ do
        addToken
        byLabel "What is your email?" username
        byLabel "Please tell us your passphrase, too." password
        setMethod "POST"
        setUrl (AuthR LoginR)

withApp :: SpecWith (TestApp App) -> Spec
withApp = before $ do
    settings <- loadYamlSettings
        ["config/test-settings.yml", "config/settings.yml"]
        []
        ignoreEnv
    -- Note: I can't drop all the tables because the migration happens in
    -- 'makeFoundation', but that's also the function that builds a usable
    -- database pool out of the app settings. So, 'makeFoundation' would need
    -- to be broken up to allow clearing before migration.
    --
    -- Clearing looks like "drop schema public cascade; create schema public;"
    --
    -- See also https://tree.taiga.io/project/snowdrift/issue/402
    truncateTables (pgConnStr (appDatabaseConf settings))
    foundation <- makeFoundation settings
    logWare <- liftIO $ makeLogWare foundation
    return (foundation, logWare)

-- This function will truncate all of the tables in your database.
-- 'withApp' calls it before each test, creating a clean environment for each
-- spec to run in.
truncateTables :: ConnectionString -> IO ()
truncateTables connstr =
    bracket (PG.connectPostgreSQL connstr) PG.close $ \conn -> do
        void $ PG.execute_ conn "set client_min_messages to warning"
        void $ PG.execute_ conn "drop schema public cascade"
        void $ PG.execute_ conn "create schema public"

-- ##

testRoot :: Text
testRoot = "" --approot is empty in `website/config/settings.yml`

-- | Assert that the given request redirects to the login page.
assertNeedsAuth :: RequestBuilder App () -> YesodExample App ()
assertNeedsAuth req = do
    request req

    authRte <- testRender (AuthR LoginR) []

    withResponse
        (\response -> do
            let code = statusCode (simpleStatus response)
            liftIO $ assertBool ("Expected a 302 or 303 redirection status "
                        <> "but received " <> show code)
                       (code `elem` [302,303])
            assertHeader "location" (T.encodeUtf8 authRte)
        )

testRender :: Route App -> [(Text, Text)] -> YesodExample App Text
testRender route params =
    yesodRender <$> getTestYesod <*> pure testRoot <*> pure route <*> pure params

htmlHasLink :: Route App -> YesodExample App ()
htmlHasLink route = do
    uri <- testRender route []
    frags <- htmlQuery ("a[href="<>uri<>"]")
    liftIO $ assertBool (X.unpack uri <> " not found") (not (null frags))
