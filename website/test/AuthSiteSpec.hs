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
/user/#Text UserR GET
/auth AuthSub AuthSite ahTestAuth
/login-bypass/#Text LoginBypass POST
/logout-bypass LogoutBypass POST
/provisional-user-bypass/#Text/#Text ProvisionalUserBypass POST
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
    resetPassphraseHandler = loginHandler

    verifyAccountHandler = do
        ((_, tokenField), enctype) <-
            runFormPost (renderDivs (areq textField "Token" Nothing))
        selectRep $ provideRep $ defaultLayout [whamlet|
            <form method="post" enctype=#{enctype}>
                ^{tokenField}
            |]

-- ** Methods for checking results

getMaybeAuth :: Handler Text
getMaybeAuth = T.pack . show <$> maybeAuth

getRequireAuth :: Handler Text
getRequireAuth = T.pack . show <$> requireAuth

getSessionVal :: Handler Text
getSessionVal = T.pack . show <$> lookupSession "_AUTHID"

getProvisional :: Handler Text
getProvisional = T.pack . show' <$> runDB (selectFirst [] [])
  where
    show' :: Maybe (Entity ProvisionalUser) -> String
    show' = show

getUserR :: Text -> Handler Text
getUserR = fmap (T.pack . show) . runDB . getBy . UniqueUsr

-- ** "Bypass" routes, which do some function without going through the
-- real workflow. I use these to make sure the different components are
-- tested independently: e.g. when testing token verification, I'll just
-- bypass the regular login.

postLoginBypass :: Text -> Handler Text
postLoginBypass e =
    "Logged in, you cheeky bastard you"
    <$ (priviligedLogin =<< runDB (getBy404 (UniqueUsr e)))

postProvisionalUserBypass :: Text -> Text -> Handler Text
postProvisionalUserBypass e p = verifyToken <$>
    runDB
        (priviligedProvisionalUser
            (Credentials (AuthEmail e) (ClearPassphrase p)))

postLogoutBypass :: Handler ()
postLogoutBypass = logout

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

-- | Create Bob before all tests
-- This is old; should probably create/use createUserBypass instead
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
-- the right spot. The problem is that it needs a different value of
-- AuthHarness.
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
            bypassLoginBob
            get MaybeAuth >> bodyContains "bob@example.com"
        it "gets nothing after logout" $ do
            bypassLoginBob
            get MaybeAuth >> bodyContains "bob@example.com"
            bypassLogout
            get MaybeAuth >> bodyEquals "Nothing"
    describe "requireAuth *without* authRoute" $ do
        it "gets the user" $ do
            get RequireAuth >> statusIs 401
            bypassLoginBob
            get RequireAuth >>
                (statusIs 200 >> bodyContains "1")
        it "errors after logout" $ do
            bypassLoginBob
            get RequireAuth >>
                (statusIs 200 >> bodyContains "1")
            bypassLogout
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
            createAA
            get Provisional >> bodyContains "alice@example.com"
        it "doesn't mind if a user asks twice" $ do
            createAA
            createAA
            get Provisional >> bodyContains "alice@example.com"
        it "doesn't create a provisional user if one already exists" $ do
            goCreate "bob@example.com" "zzzzzzzzzzzzz"
            get Provisional >> bodyEquals "Nothing"
    describe "VerifyAccountR" $ do
        it "creates an account with a good token" $ do
            v <- bypassProvisionalAA
            get (AuthSub VerifyAccountR)
            request $ do
                addToken
                byLabel "Token" (TL.toStrict v)
                setMethod "POST"
                setUrl (AuthSub VerifyAccountR)
            get (UserR "alice@example.com") >> bodyContains "alice@example.com"
        it "doesn't add something it shouldn't" $ do
            v <- bypassProvisionalAA
            get (AuthSub VerifyAccountR)
            request $ do
                addToken
                byLabel "Token" (TL.toStrict (v <> "garbage"))
                setMethod "POST"
                setUrl (AuthSub VerifyAccountR)
            get (UserR "alice@example.com") >> bodyEquals "Nothing"
    describe "ResetPassphraseR" $ do
        it "makes a provisional user if the user already exists" $ do
            goForget "bob@example.com" "ccccccccccccc"
            get Provisional >> bodyContains "bob@example.com"
        it "doesn't mind if a person asks twice" $ do
            goForget "bob@example.com" "ccccccccccccc"
            goForget "bob@example.com" "ccccccccccccc"
            get Provisional >> bodyContains "bob@example.com"
        it "doesn't make a provisional user if the user dne" $ do
            goForget "alice@example.com" "ccccccccccccc"
            get Provisional >> bodyEquals "Nothing"
  where
    createAA = goCreate "alice@example.com" "aaaaaaaaaaaaa"
    goForget e p = do
        get (AuthSub ResetPassphraseR)
        request $ do
            addToken
            byLabel "Email" e
            byLabel "Passphrase" p
            setMethod "POST"
            setUrl (AuthSub ResetPassphraseR)
        statusIs 303
    goCreate e p = do
        get (AuthSub CreateAccountR)
        request $ do
            addToken
            byLabel "Email" e
            byLabel "Passphrase" p
            setMethod "POST"
            setUrl (AuthSub CreateAccountR)
        statusIs 303
    bypassProvisionalAA = do
        post (ProvisionalUserBypass "alice@example.com" "aaaaaaaaaaaaa")
        Just resp <- getResponse
        pure (decodeUtf8 (simpleBody resp))

    bypassLoginBob = bypassLogin "bob@example.com"
    bypassLogin :: Text -> AuthExample ()
    bypassLogin = post . LoginBypass
    bypassLogout = post LogoutBypass

createUser :: AuthEmail -> ClearPassphrase -> SqlPersistM ()
createUser e p = do
    ProvisionalUser{..} <-
        liftIO $ provisional (Credentials e p) (Verification e "stuff")
    privilegedCreateUser (VerifiedUser provisionalEmail provisionalDigest)
