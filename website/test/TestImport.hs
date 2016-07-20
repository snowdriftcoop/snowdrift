module TestImport
    ( module TestImport
    , module X
    ) where

import ClassyPrelude as X
import Database.Persist as X hiding (get)
import Database.Persist.Sql (SqlPersistM, SqlBackend, runSqlPersistMPool, rawExecute, rawSql, unSingle, connEscapeName)
import Foundation as X
import Network.HTTP.Types (Status(..), Method)
import Network.Wai.Test (SResponse(..))
import Test.Hspec as X
import Text.Shakespeare.Text (st)
import Yesod.Default.Config2 (ignoreEnv, loadAppSettings)
import Yesod.Test as X
import qualified Data.Text.Encoding as T

-- For htmlHasLink
import Test.HUnit
import Yesod.Core

import Application (makeFoundation, makeLogWare)
import AppTypes as X
import AuthSite
import Model as X

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
    _ <- testDB $ createUser "alice" "ccccccccccccc"
    get (AuthR LoginR)
    request $ do
        addToken
        byLabel "Email" "alice"
        byLabel "Passphrase" "ccccccccccccc"
        setMethod "POST"
        setUrl (AuthR LoginR)

withApp :: SpecWith (TestApp App) -> Spec
withApp = before $ do
    settings <- loadAppSettings
        ["config/test-settings.yml", "config/settings.yml"]
        []
        ignoreEnv
    foundation <- makeFoundation settings
    wipeDB foundation
    logWare <- liftIO $ makeLogWare foundation
    return (foundation, logWare)

-- This function will truncate all of the tables in your database.
-- 'withApp' calls it before each test, creating a clean environment for each
-- spec to run in.
wipeDB :: App -> IO ()
wipeDB app = runDBWithApp app $ do
    tables <- getTables
    sqlBackend <- ask

    let escapedTables = map (connEscapeName sqlBackend . DBName) tables
        query = "TRUNCATE TABLE " ++ intercalate ", " escapedTables
    rawExecute query []

getTables :: MonadIO m => ReaderT SqlBackend m [Text]
getTables = do
    tables <- rawSql [st|
        SELECT table_name
        FROM information_schema.tables
        WHERE table_schema = 'public';
    |] []

    return $ map unSingle tables

-- ##

testRoot :: Text
testRoot = "http://localhost:3000"

needsAuth :: Route App -> Method -> YesodExample App ()
needsAuth route method = do
    authRte <- testRender (AuthR LoginR) []
    request $ do
        setMethod method
        setUrl route
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
