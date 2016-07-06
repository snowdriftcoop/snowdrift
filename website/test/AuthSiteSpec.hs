{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
module AuthSiteSpec (spec) where

import TestImport hiding (Handler)
import Database.Persist.Sql hiding (get)
import Network.Wai.Test (SResponse(..))
import Yesod hiding (get)
import Yesod.Default.Config2 (ignoreEnv, loadAppSettings)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import AuthSite
import Application (makeFoundation, makeLogWare)

-- ** A harness for the auth subsite

data AuthHarness = AuthHarness
    { ahTestAuth :: AuthSite
    , ahConnPool :: ConnectionPool
    , ahAuthRoute :: Maybe (Route AuthHarness)
    }

mkYesod "AuthHarness" [parseRoutes|
/provisional Provisional GET
/maybe-auth MaybeAuth GET
/require-auth RequireAuth GET
/session-val SessionVal GET
/auth AuthSub AuthSite ahTestAuth
/dummy-login/#Text DummyLogin GET
/dummy-provisional/#Text/#Text DummyProvisional GET
|]

instance AuthMaster AuthHarness where
    postLoginRoute _ = MaybeAuth
    postLogoutRoute = postLoginRoute

    loginHandler = do
        ((_, loginFields), enctype) <- runFormPost (renderDivs credentialsForm)
        selectRep $ provideRep $ defaultLayout [whamlet|
            <form method="post" enctype=#{enctype}>
                ^{loginFields}
            |]

    createAccountHandler = loginHandler

getMaybeAuth :: Handler Text
getMaybeAuth = T.pack . show <$> maybeAuth

getRequireAuth :: Handler Text
getRequireAuth = T.pack . show <$> requireAuth

getSessionVal :: Handler Text
getSessionVal = T.pack . show <$> lookupSession "_AUTHID"

getProvisional  :: Handler Text
getProvisional = T.pack . show' <$> runDB (selectFirst [] [])
  where
    show' :: Maybe (Entity ProvisionalUser) -> String
    show' = show

getDummyLogin :: Text -> Handler Text
getDummyLogin e =
    "Logged in, you cheeky bastard you"
    <$ (priviligedLogin =<< runDB (getBy404 (UniqueUsr e)))

getDummyProvisional :: Text -> Text -> Handler Text
getDummyProvisional e p = verifyToken <$>
    runDB
        (priviligedProvisionalUser
            (Credentials (AuthEmail e) (ClearPassphrase p)))

-- ** Boilerplate for the harness site

instance Yesod AuthHarness where
    authRoute = ahAuthRoute

instance YesodPersist AuthHarness where
    type YesodPersistBackend AuthHarness = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action (ahConnPool master)

instance RenderMessage AuthHarness FormMessage where
    renderMessage _ _ = defaultFormMessage


-- ** Boilerplate to run Specs with this harness.

withTestAuth :: Maybe (Route AuthHarness)
             -> SpecWith (TestApp AuthHarness)
             -> Spec
withTestAuth maybeAuthRoute = before $ do
    settings <- loadAppSettings
        ["config/test-settings.yml", "config/settings.yml"]
        []
        ignoreEnv
    foundation <- makeFoundation settings
    wipeDB foundation
    logWare <- liftIO $ makeLogWare foundation
    let harness = AuthHarness AuthSite
                              (appConnPool foundation)
                              maybeAuthRoute
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
spec = mainSpecs >> authRouteSpec

-- | Having this defined separately is clumsy. It should be moved back into
-- the right spot.
authRouteSpec :: Spec
authRouteSpec = withTestAuth (Just MaybeAuth) $
    describe "requireAuth *with* authRoute" $
        it "redirects" $ do
            get RequireAuth
            statusIs 303
            assertHeader "location" "/maybe-auth"

mainSpecs :: Spec
mainSpecs = withTestAuth Nothing $ withBob $ do
    describe "maybeAuth" $ do
        it "gets the user" $ do
            get MaybeAuth >> bodyEquals "Nothing"
            dummyLoginBob
            get MaybeAuth >> bodyContains "bob@example.com"
        it "gets nothing after logout" $ do
            dummyLoginBob
            get MaybeAuth >> bodyContains "bob@example.com"
            goLogout
            get MaybeAuth >> bodyEquals "Nothing"
    describe "session key" $ do
        it "is set on login" $ do
            get SessionVal >> bodyEquals "Nothing"
            dummyLoginBob
            get SessionVal >> bodyContains "Just"
        it "is cleared on logout" $ do
            dummyLoginBob
            get SessionVal >> bodyContains "Just"
            goLogout
            get SessionVal >> bodyEquals "Nothing"
    describe "requireAuth *without* authRoute" $ do
        it "gets the user" $ do
            get RequireAuth >> statusIs 401
            dummyLoginBob
            get RequireAuth >>
                (statusIs 200 >> bodyContains "1")
        it "errors after logout" $ do
            dummyLoginBob
            get RequireAuth >>
                (statusIs 200 >> bodyContains "1")
            goLogout
            get RequireAuth >> statusIs 401
    describe "getLoginR" $ do
        it "logs a body in" $ do
            get (AuthSub LoginR)
            request $ do
                addToken
                byLabel "Email" "bob@example.com"
                byLabel "Passphrase" "aaaaaaaaaaaaa"
                setMethod "POST"
                setUrl (AuthSub LoginR)
            Right _ <- followRedirect
            bodyContains "bob@example.com"
        it "spot check - bad passphrase" $ do
            get (AuthSub LoginR)
            request $ do
                addToken
                byLabel "Email" "bob@example.com"
                byLabel "Passphrase" "bbbbbbbbbbbbb"
                setMethod "POST"
                setUrl (AuthSub LoginR)
            assertHeader "location" "/auth/login"
            Right _ <- followRedirect
            get SessionVal >> bodyEquals "Nothing"
    describe "CreateAccountR" $ do
        it "creates a new ProvisionalUser" $ do
            get Provisional >> bodyEquals "Nothing"
            provisionalAA
            get Provisional >> bodyContains "a@example.com"
        it "doesn't mind if a user is already provisional" $ do
            provisionalAA
            provisionalAA
            get Provisional >> bodyContains "a@example.com"
    describe "VerifyAccountR" $ do
        it "creates an account with a good token" $ do
            v <- dummyProvisionalAA
            get (AuthSub VerifyAccountR)
            request $ do
                addToken
                byLabel "Token" (TL.toStrict v)
                setMethod "POST"
                setUrl (AuthSub VerifyAccountR)
            get (AuthSub LoginR)
            request $ do
                addToken
                byLabel "Email" "a@example.com"
                byLabel "Passphrase" "aaaaaaaaaaaaa"
                setMethod "POST"
                setUrl (AuthSub LoginR)
            Right _ <- followRedirect
            bodyContains "a@example.com"
  where
    provisionalAA = do
        get (AuthSub CreateAccountR)
        request $ do
            addToken
            byLabel "Email" "a@example.com"
            byLabel "Passphrase" "aaaaaaaaaaaaa"
            setMethod "POST"
            setUrl (AuthSub CreateAccountR)
    dummyProvisionalAA = do
        get (DummyProvisional "a@example.com" "aaaaaaaaaaaaa")
        Just resp <- getResponse
        pure (decodeUtf8 (simpleBody resp))

    dummyLoginBob = dummyLogin "bob@example.com"
    dummyLogin :: Text -> AuthExample ()
    dummyLogin = get . DummyLogin
    goLogout = post (AuthSub LogoutR)

createUser :: AuthEmail -> ClearPassphrase -> SqlPersistM ()
createUser e p = do
    ProvisionalUser{..} <-
        liftIO $ provisional (Credentials e p) (Verification e "stuff")
    privilegedCreateUser (VerifiedUser provisionalEmail provisionalDigest)
