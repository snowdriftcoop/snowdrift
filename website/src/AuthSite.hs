{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Subsite for email-and-passphrase authentication.
module AuthSite (module AuthSiteTypes, module AuthSite) where

import Prelude

import Control.Lens
import Crypto.PasswordStore
import Data.ByteString (ByteString)
import Control.Monad
import Data.Text (Text)
import Data.Text.Encoding
import Data.Time
import Database.Persist.Sql
import Crypto.Nonce (Generator, nonce128urlT)
import Yesod
import qualified Crypto.Nonce as Nonce

-- Used to create a single, applicationd-wide nonce token. I'm doing this
-- out of expediency; Yesod.Auth uses it and nobody seems to care. But I
-- don't like it.
-- https://github.com/yesodweb/yesod/issues/1245
import System.IO.Unsafe (unsafePerformIO)

import Alerts
import AuthSiteTypes
import Css
import Model
import Settings

-- To create a usable API, AuthUser would have to be added to an exposed
-- typeclass.
type AuthUser = Entity User

-- ** Internal types. Sequestered up here to appease the TH gods.

newtype AuthEmail = AuthEmail { fromAuth :: Text } deriving Show

newtype ClearPassphrase = ClearPassphrase { fromClear :: Text } deriving Show

newtype PassphraseDigest = PassphraseDigest ByteString deriving Show

data Credentials = Credentials
        { loginAuth :: AuthEmail
        , loginPass :: ClearPassphrase
        } deriving (Show)

data VerifiedUser = VerifiedUser
        { verifiedEmail :: Text
        , verifiedDigest :: ByteString
        }

data Verification = Verification
        { verifyEmail :: Text
        , verifyToken :: Text
        }

-- ** Invoke Yesod TH to make the subsite.

instance (Yesod master
         ,YesodPersist master
         ,YesodPersistBackend master ~ SqlBackend
         ,RedirectUrl master r
         ,RenderMessage master FormMessage)
        => YesodSubDispatch (AuthSite r) (HandlerT master IO) where
    yesodSubDispatch = $(mkYesodSubDispatch resourcesAuthSite)

-- ** Duplicating Yesod.Auth API for a wee while.

maybeAuth :: HandlerT m IO (Maybe AuthUser)
maybeAuth = pure Nothing

requireAuth :: HandlerT m IO AuthUser
requireAuth = pure $ Entity dummyKey (User "foo" "bar")
  where dummyKey = fromRight $ keyFromValues [PersistInt64 1]
        fromRight (Right x) = x
        fromRight _ = error "Dastardly partiality"


-- ** Functions and operations for doing auth

authSessionKey :: Text
authSessionKey = "_AUTHID"

-- Per the docs, this number should increase by 1 every two years, starting
-- at 17 in 2014. Thus, 17 + (now^.year - 2014) / 2. We could even TH that
-- bizniss.
--
-- Ok, I TH'd it. Will I regret it? Yes. Leaving it commented for
-- now.
pbkdf1Strength :: Int
pbkdf1Strength = 18
-- pbkdf1Strength = 17 + (yr - 2014) `div` 2
--   where yr = $(litE =<< runIO (fmap ( IntegerL
--                                     . (\(a,_,_) -> a)
--                                     . toGregorian
--                                     . utctDay)
--                                     getCurrentTime))

-- | Yesod.Auth uses this, and it's apparently ok.
-- https://github.com/yesodweb/yesod/issues/1245
tokenGenerator :: Generator
tokenGenerator = unsafePerformIO Nonce.new
{-# NOINLINE tokenGenerator #-}

-- | Compare some Credentials to what's stored in the database.
checkCredentials :: MonadIO m => Credentials -> SqlPersistT m (Maybe AuthUser)
checkCredentials Credentials{..} = do
    mu <- getBy (UniqueUsr (fromAuth loginAuth))
    pure $ verify =<< mu
  where
    verify x@(Entity uid u) =
        if verifyPassword (encodeUtf8 (fromClear loginPass)) (u^.userDigest)
            then Just x
            else Nothing

-- | Store a provisional user for later verification. Returns the token to
-- use for verification.
storeProvisionalUser :: MonadIO m => Credentials -> SqlPersistT m Verification
storeProvisionalUser creds = do
    tok <- genVerificationToken creds
    insert_ =<< provisional creds tok
    pure tok

-- | Create a provisional user
provisional :: MonadIO m
            => Credentials -> Verification -> m ProvisionalUser
provisional Credentials{..} Verification{..} = liftIO $
    ProvisionalUser <$> email <*> passDigest <*> tokDigest <*> curtime
  where
    email = pure (fromAuth loginAuth)
    passDigest = makePass (fromClear loginPass)
    tokDigest = makePass verifyToken
    curtime = getCurrentTime
    makePass t = makePassword (encodeUtf8 t) pbkdf1Strength

genVerificationToken :: MonadIO m => Credentials -> m Verification
genVerificationToken Credentials{..} =
    Verification (fromAuth loginAuth) <$> nonce128urlT tokenGenerator

-- | This privileged function must be used with care.
privilegedCreateUser :: MonadIO m => VerifiedUser -> SqlPersistT m AuthUser
privilegedCreateUser VerifiedUser{..} =
    insertEntity (User verifiedEmail verifiedDigest)

-- | This privileged function must be used with care. It modifies the
-- user's session; it's the difference between being logged in and not!
priviligedLogin :: Yesod master => AuthUser -> HandlerT master IO ()
priviligedLogin = setSession authSessionKey . toPathPiece . entityKey

-- | Log the user out.
logout :: Yesod master => HandlerT master IO ()
logout = deleteSession authSessionKey

-- ** Now building the login page.

loginForm :: (RenderMessage (HandlerSite m) FormMessage, MonadHandler m)
          => AForm m Credentials
loginForm = Credentials
    <$> (AuthEmail <$> areq textField "Email"{fsAttrs=emailAttrs}  Nothing)
    <*> (ClearPassphrase <$> areq passwordField "Passphrase" Nothing)
  where
    emailAttrs = [("autofocus",""), ("autocomplete","email")]

getLoginR :: (Yesod master, RenderMessage master FormMessage)
          => HandlerT (AuthSite r) (HandlerT master IO) Html
getLoginR = lift $ do
    (loginFields, enctype) <- generateFormPost (renderDivs loginForm)
    defaultLayout $ do
        setTitle "Login — Snowdrift.coop"
        $(widgetFile "page/auth/login")

postLoginR :: (Yesod master
              ,YesodPersist master
              ,YesodPersistBackend master ~ SqlBackend
              ,RedirectUrl master r
              ,RenderMessage master FormMessage)
           => HandlerT (AuthSite r) (HandlerT master IO) Html
postLoginR = do
    ((res', _), _) <- lift $ runFormPost (renderDivs loginForm)
    p <- getRouteToParent
    AuthSite home <- getYesod
    formResult (lift . (runAuthResult home p <=< (runDB . checkCredentials)))
               loginAgain
               res'
  where
    runAuthResult home parent = maybe
        (do
            alertDanger [shamlet|Bad credentials:  <a href="https://tree.taiga.io/project/snowdrift/us/392">See Taiga #392</a>.|]
            redirect (parent LoginR))
        (\u -> do
            priviligedLogin u
            alertInfo "Welcome"
            redirect home)
    loginAgain msgs = do
        lift $ defaultLayout [whamlet|
            <p>Login form failures are not handled yet.
            <p>TBD: <a href="https://tree.taiga.io/project/snowdrift/us/392">See Taiga #392</a>.
            <p>Errors: #{show msgs}
            |]

    formResult success failure = \case
        FormSuccess x -> success x
        FormFailure msgs -> failure msgs
        FormMissing -> failure ["No login data"]


postLogoutR :: Yesod master => HandlerT (AuthSite r) (HandlerT master IO) Html
postLogoutR = undefined
