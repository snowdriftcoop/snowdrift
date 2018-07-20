{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
module AuthSiteSpec (spec_authSite) where

import TestImport hiding (Handler)
import Database.Persist.Sql hiding (get)
import Database.Persist.Postgresql (pgConnStr)
import Network.Wai.Test (SResponse(..))
import Test.Tasty.HUnit (assertBool)
import Yesod hiding (get)
import Yesod.Default.Config2 (ignoreEnv, loadYamlSettings)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import AuthSite
import Application (makeFoundation, makeLogWare)
import Settings (AppSettings(..))

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

-- ** A harness for the auth subsite

data AuthHarness = AuthHarness
    { ahTestAuth :: AuthSite
    , ahConnPool :: ConnectionPool
    , ahAuthRoute :: Maybe (Route AuthHarness)
    , ahMailMessage :: IORef [AuthMailMessage]
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
/mailmessage MailMessage GET
/provisional-user-bypass/#Text/#Text ProvisionalUserBypass POST
|]

instance AuthMaster AuthHarness where
    postLoginRoute _ = MaybeAuth
    postLogoutRoute = postLoginRoute

    loginHandler = do
        ((_, loginFields), enctype) <- runFormPost (renderDivs credentialsForm)
        defaultLayout [whamlet|
            <form method="post" enctype=#{enctype}>
                ^{loginFields}
            |]

    createAccountHandler = loginHandler
    resetPassphraseHandler = loginHandler

    verifyAccountHandler = do
        ((_, tokenField), enctype) <-
            runFormPost (renderDivs (areq textField "Token" Nothing))
        defaultLayout [whamlet|
            <form method="post" enctype=#{enctype}>
                ^{tokenField}
            |]

    sendAuthEmail _ mm = do
        y <- getYesod
        liftIO $ modifyIORef (ahMailMessage y) (mm :)

harnessDB :: SqlPersistM a -> YesodExample AuthHarness a
harnessDB query = do
    ah <- getTestYesod
    liftIO (runSqlPersistMPool query (ahConnPool ah))

-- ** Methods for checking results

getMaybeAuth :: Handler Text
getMaybeAuth = T.pack . show <$> maybeAuth

getRequireAuth :: Handler Text
getRequireAuth = T.pack . show <$> requireAuth

getSessionVal :: Handler Text
getSessionVal = T.pack . show <$> lookupSession "_AUTHID"

getProvisional :: Handler Text
getProvisional =
    T.intercalate ", " . map extractEmail <$> runDB (selectList [] [])
  where
    extractEmail = provisionalUserEmail . entityVal

getUserR :: Text -> Handler Text
getUserR = fmap (T.pack . show) . runDB . getBy . UniqueUsr

getMailMessage :: Handler Text
getMailMessage = do
    y <- getYesod
    m <- liftIO (readIORef (ahMailMessage y))
    pure (T.pack (show m))

-- ** "Bypass" routes, which do some function without going through the
-- real workflow. I use these to make sure the different components are
-- tested independently: e.g. when testing token verification, I'll just
-- bypass the regular login.

postLoginBypass :: Text -> Handler Text
postLoginBypass e =
    "Logged in, you cheeky bastard you"
    <$ (privilegedLogin =<< runDB (getBy404 (UniqueUsr e)))

postProvisionalUserBypass :: Text -> Text -> Handler Text
postProvisionalUserBypass e p = fromAuthToken . verifyToken <$>
    runDB
        (privilegedProvisionalUser
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
    settings <- loadYamlSettings
        ["config/test-settings.yml", "config/settings.yml"]
        []
        ignoreEnv
    truncateTables (pgConnStr (appDatabaseConf settings))
    foundation <- makeFoundation settings
    msgref <- liftIO (newIORef [])
    logWare <- liftIO $ makeLogWare foundation
    let harness = AuthHarness AuthSite
                              (appConnPool foundation)
                              maybeAuthRoute
                              msgref
    return (harness, logWare)

type AuthExample a = YesodExample AuthHarness a

-- ** Some local tools

-- | Create Bob before all tests
withBob :: SpecWith (TestApp AuthHarness) -> SpecWith (TestApp AuthHarness)
withBob = beforeWith makeBob
  where
    makeBob stuff@(harness, _) =
       runSqlPersistMPool goBobGo (ahConnPool harness) >> pure stuff
    goBobGo = privilegedCreateUserBypass (AuthEmail "bob@example.com")
                                         (ClearPassphrase "aaaaaaaaaaaaa")

-- ** The actual tests!

spec_authSite :: Spec
spec_authSite = mainSpecs >> authRouteSpec

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
                fillCredentialsForm "bob@example.com" "aaaaaaaaaaaaa"
                setUrl (AuthSub LoginR)
            Right _ <- followRedirect
            bodyContains "bob@example.com"
        it "spot check - bad passphrase" $ do
            get (AuthSub LoginR)
            request $ do
                fillCredentialsForm "bob@example.com" "bbbbbbbbbbbbb"
                setUrl (AuthSub LoginR)
            assertHeader "location" "/auth/login"
            Right _ <- followRedirect
            get SessionVal >> bodyEquals "Nothing"
    describe "CreateAccountR" $ do
        it "creates a new ProvisionalUser" $ do
            get Provisional >> bodyEquals ""
            createAA
            get Provisional >> bodyEquals "alice@example.com"
        it "doesn't mind if a user asks twice" $ do
            createAA
            createAA
            get Provisional >> bodyEquals "alice@example.com"
        it "doesn't create a provisional user if one already exists" $ do
            goCreate "bob@example.com" "zzzzzzzzzzzzz"
            get Provisional >> bodyEquals ""
        it "sends VerifyUserCreation" $ do
            createAA
            get MailMessage >> bodyContains "[VerifyUserCreation"
        it "sends BadUserCreation" $ do
            goCreate "bob@example.com" "zzzzzzzzzzzzz"
            get MailMessage >> bodyContains "[BadUserCreation"
        it "requires 9 characters in the passphrase" $ do
            goCreate "bob1@example.com" "12345678"
            get Provisional >> bodyEquals ""
            goCreate "bob2@example.com" "123456789"
            get Provisional >> bodyEquals "bob2@example.com"

    describe "VerifyAccountR" $ do
        it "creates an account with a good token" $ do
            v <- bypassProvisionalAA
            get (AuthSub VerifyAccountR)
            request $ do
                fillTokenForm v
                setUrl (AuthSub VerifyAccountR)
            get (UserR "alice@example.com") >> bodyContains "alice@example.com"
        it "doesn't add something it shouldn't" $ do
            v <- bypassProvisionalAA
            get (AuthSub VerifyAccountR)
            request $ do
                fillTokenForm (v <> "garbage")
                setUrl (AuthSub VerifyAccountR)
            get (UserR "alice@example.com") >> bodyEquals "Nothing"
        it "updates a passphrase with a good token" $ do
            [Entity b1 bob1] :: [Entity User] <- harnessDB (selectList [] [])
            v <- bypassProvisionalBob
            get (AuthSub VerifyAccountR)
            request $ do
                fillTokenForm v
                setUrl (AuthSub VerifyAccountR)
            [Entity b2 bob2] :: [Entity User] <- harnessDB (selectList [] [])

            assertEq "ID changed" b1 b2
            liftIO (assertBool "Pass update time did not change"
                               (((/=) `on` _userPassUpdated) bob1 bob2))
            liftIO (assertBool "Passphrases did not change"
                               (((/=) `on` _userDigest) bob1 bob2))
        it "doesn't update a passphrase it shouldn't" $ do
            [Entity b1 bob1] :: [Entity User] <- harnessDB (selectList [] [])
            v <- bypassProvisionalBob
            get (AuthSub VerifyAccountR)
            request $ do
                fillTokenForm (v <> "oops")
                setUrl (AuthSub VerifyAccountR)
            [Entity b2 bob2] :: [Entity User] <- harnessDB (selectList [] [])

            assertEq "ID stayed the same" b1 b2
            liftIO (assertBool "Pass update time changed"
                               (((==) `on` _userPassUpdated) bob1 bob2))
            liftIO (assertBool "Passphrases changed"
                               (((==) `on` _userDigest) bob1 bob2))
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
            get Provisional >> bodyEquals ""
        it "sends VerifyPassReset" $ do
            goForget "bob@example.com" "ccccccccccccc"
            get MailMessage >> bodyContains "[VerifyPassReset"
        it "sends BadPassReset" $ do
            goForget "alice@example.com" "ccccccccccccc"
            get MailMessage >> bodyContains "[BadPassReset"
  where
    createAA = goCreate "alice@example.com" "aaaaaaaaaaaaa"
    goForget e p = do
        get (AuthSub ResetPassphraseR)
        request $ do
            fillCredentialsForm e p
            setUrl (AuthSub ResetPassphraseR)
        statusIs 303
    goCreate e p = do
        get (AuthSub CreateAccountR)
        request $ do
            fillCredentialsForm e p
            setUrl (AuthSub CreateAccountR)
    bypassProvisionalAA = bypassProvisional "alice@example.com" "aaaaaaaaaaaaa"
    bypassProvisionalBob = bypassProvisional "bob@example.com" "ccccccccccccc"
    bypassProvisional e p = do
        post (ProvisionalUserBypass e p)
        Just resp <- getResponse
        pure (decodeUtf8 (simpleBody resp))

    fillCredentialsForm e p = do
        addToken
        byLabel "Email" e
        byLabel "Passphrase" p
        setMethod "POST"
    fillTokenForm t = do
        addToken
        byLabel "Token" (TL.toStrict t)
        setMethod "POST"

    bypassLoginBob = bypassLogin "bob@example.com"
    bypassLogin :: Text -> AuthExample ()
    bypassLogin = post . LoginBypass
    bypassLogout = post LogoutBypass
