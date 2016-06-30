{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
module AuthSiteSpec (spec) where

import TestImport hiding (Handler)
import Database.Persist.Sql hiding (get)
import Yesod hiding (get)
import Yesod.Default.Config2 (ignoreEnv, loadAppSettings)
import qualified Data.Text as T

import AuthSite
import Application (makeFoundation, makeLogWare)

-- ** A harness for the auth subsite

data AuthHarness = AuthHarness
    { ahTestAuth :: (AuthSite (Route AuthHarness))
    , ahConnPool :: ConnectionPool
    }

mkYesod "AuthHarness" [parseRoutes|
/maybe-auth MaybeAuth GET
/require-auth RequireAuth GET
/session-val SessionVal GET
/auth AuthSub AuthSite-(Route-AuthHarness) ahTestAuth
|]

getMaybeAuth :: Handler Text
getMaybeAuth = T.pack . show <$> maybeAuth

getRequireAuth :: Handler Text
getRequireAuth = T.pack . show <$> requireAuth

getSessionVal :: Handler Text
getSessionVal = T.pack . show <$> lookupSession "_AUTHID"

-- ** Boilerplate for the harness site

instance Yesod AuthHarness

instance YesodPersist AuthHarness where
    type YesodPersistBackend AuthHarness = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action (ahConnPool master)

instance RenderMessage AuthHarness FormMessage where
    renderMessage _ _ = defaultFormMessage


-- ** Boilerplate to run Specs with this harness.

withTestAuth :: SpecWith (TestApp AuthHarness) -> Spec
withTestAuth = before $ do
    settings <- loadAppSettings
        ["config/test-settings.yml", "config/settings.yml"]
        []
        ignoreEnv
    foundation <- makeFoundation settings
    wipeDB foundation
    logWare <- liftIO $ makeLogWare foundation
    let harness = AuthHarness (AuthSite MaybeAuth) (appConnPool foundation)
    return (harness, logWare)

type AuthExample a = YesodExample AuthHarness a

-- ** Some local tools

-- | Run db actions with this harness
harnessDB :: SqlPersistM a -> AuthExample a
harnessDB query = do
    h <- getTestYesod
    liftIO $ runSqlPersistMPool query (ahConnPool h)

-- | Create Bob before all tests
withBob :: SpecWith (TestApp AuthHarness) -> SpecWith (TestApp AuthHarness)
withBob = beforeWith makeBob
  where
    makeBob stuff@(harness, _) =
       runSqlPersistMPool goBobGo (ahConnPool harness) >> pure stuff
    goBobGo = createUser (AuthEmail "bob@example.com")
                         (ClearPassphrase "aaaaaaaaaaaaa")

-- ** The actual tests!

spec :: Spec
spec = withTestAuth $ withBob $ do
    describe "maybeAuth" $ do
        it "gets the user" $ do
            get MaybeAuth >> bodyContains "Nothing"
            loginBob
            get MaybeAuth >> bodyContains "bob@example.com"
        it "gets nothing after logout" $ do
            loginBob
            get MaybeAuth >> bodyContains "bob@example.com"
            logout
            get MaybeAuth >> bodyContains "Nothing"
    describe "session key" $ do
        it "is set on login" $ do
            get SessionVal >> bodyContains "Nothing"
            loginBob
            get SessionVal >> bodyContains "Just"
        it "is cleared on logout" $ do
            loginBob
            get SessionVal >> bodyContains "Just"
            logout
            get SessionVal >> bodyContains "Nothing"
    describe "requireAuth" $ do
        it "gets the user" $ do
            get RequireAuth >> statusIs 401
            loginBob
            get RequireAuth >>
                (statusIs 200 >> bodyContains "1")
        it "errors after logout" $ do
            loginBob
            get RequireAuth >>
                (statusIs 200 >> bodyContains "1")
            logout
            get RequireAuth >> statusIs 401
    it "times out after two hours" $ do
        loginBob
        error "Pending test"
  where
    loginBob = loginAs "bob@example.com" "aaaaaaaaaaaaa"
    loginAs :: Text -> Text -> AuthExample ()
    loginAs _ _ = pure ()
    logout = post (AuthSub LogoutR)

createUser :: AuthEmail -> ClearPassphrase -> SqlPersistM ()
createUser _ _ = pure ()
