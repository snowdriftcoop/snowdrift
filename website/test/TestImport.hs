module TestImport
    ( module TestImport
    , module X
    ) where

import ClassyPrelude         as X
import Database.Persist      as X hiding (get)
import Database.Persist.Sql  (SqlPersistM, SqlBackend, runSqlPersistMPool, rawExecute, rawSql, unSingle, connEscapeName)
import Foundation            as X
import Test.Hspec            as X
import Text.Shakespeare.Text (st)
import Yesod.Default.Config2 (ignoreEnv, loadAppSettings)
import Yesod.Auth (Route(..))
import Yesod.Test            as X
import Network.Wai.Test (SResponse(..))
import Network.HTTP.Types (Status(..), Method)
import qualified Data.Text.Encoding as T

import Application           (makeFoundation, makeLogWare)
import Model                 as X
import Factories

-- For htmlHasLink
import Yesod.Core
import Test.HUnit

runDB :: SqlPersistM a -> YesodExample App a
runDB query = do
    app <- getTestYesod
    liftIO $ runDBWithApp app query

runDBWithApp :: App -> SqlPersistM a -> IO a
runDBWithApp app query = runSqlPersistMPool query (appConnPool app)


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

login :: YesodExample App ()
login = do
    let ident = "pat"
    _ <- runDB (createUser ident)

    let url = testRoot <> "/auth/page/dummy"

    request $ do
        setMethod "POST"
        addPostParam "ident" ident
        setUrl url

needsAuth :: RedirectUrl App url => url -> Method -> YesodExample App ()
needsAuth route method = do
    authRte <- renderRoute' (AuthR LoginR)
    request $ do
        setMethod method
        setUrl route
    withResponse
        (\response -> do
            let code = statusCode (simpleStatus response)
            liftIO $ assertBool ("Expected a 302 or 303 redirection status "
                        <> "but received " <> show code)
                       (code `elem` [302,303])
            assertHeader "location" (T.encodeUtf8 (testRoot <> authRte))
        )

-- MOVE UPSTREAM

renderRoute' :: Route App -> YesodExample App Text
renderRoute' route = do
    app <- getTestYesod
    liftIO $ unsafeHandler app $ do
        r <- getUrlRender
        return (r route)

htmlHasLink :: Route App -> YesodExample App ()
htmlHasLink route = do
    uri <- renderRoute' route
    frags <- htmlQuery ("a[href="<>uri<>"]")
    liftIO $ assertBool "Link not found" (length frags > 0)
