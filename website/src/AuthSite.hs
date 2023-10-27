{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant <$>" #-}

{- |
Description : Subsite for email-and-passphrase authentication
License     : AGPL-3
Maintainer  : dev@lists.snowdrift.coop

Subsite for email-and-passphrase authentication.

Quickstart:

1. Add 'AuthSite' as a subsite to your routes somewhere.
2. Mappend 'migrateAuthSite' to your site's 'Migration' when running migrations.
3. Have your site datatype instantiate 'AuthMaster' (this is the hard part).

This module is not yet fit for general consumption. It is tightly coupled
to some Snowdrift types and functions.
-}
module AuthSite
    ( -- * Subsite interface
      -- ** Integration with your site
      AuthSite(..)
    , migrateAuthSite
    , AuthMaster(..)
      -- ** Routes
      -- | Haddock seems to dump the entire 'Route' definition, even
      -- though the only interesting thing here is the AuthSite instance
      -- with LoginR, LogoutR, CreateAccountR, VerifyAccountR, and
      -- ResetPassphraseR.
    , Route(..)
      -- ** Querying auth status
      -- | Duplicating the "Yesod.Auth" interface
    , maybeAuth
    , requireAuth
      -- * Helpers for instantiating 'AuthMaster'
    , credentialsForm
    , Credentials(..)
    , AuthEmail(..)
    , ClearPassphrase(..)
    , AuthMailMessage(..)
    , AuthToken(..)
      -- * Internals
      -- | These are exported for use in tests. You probably don't want to
      -- use them yourself.
    , AuthUser
    , provisional
    , logout
    , privilegedProvisionalUser
    , privilegedCreateUserBypass
    , privilegedLogin
    , VerifiedUser(..)
    , Verification(..)
      -- ** @persistent@-generated types
    , ProvisionalUser(..)
    ) where

import Prelude

import Control.Error
import Control.Lens
import Control.Monad
import Crypto.Nonce (Generator, nonce128urlT)
import Crypto.PasswordStore
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding
import Data.Time
import Data.Typeable
import Database.Persist.Sql
import Yesod
import qualified Crypto.Nonce as Nonce
import qualified Data.Text as T

-- Used to create a single, application-wide nonce token. I'm doing this
-- out of expediency; Yesod.Auth uses it and nobody seems to care. But I
-- don't like it.
-- https://github.com/yesodweb/yesod/issues/1245
import System.IO.Unsafe (unsafePerformIO)

import AuthSiteDataTypes

-- Need this until we switch back to using messages instead of
-- Yesod-specific "alerts".
import Alerts

-- Still need this until we take the time to put AuthUser into the
-- AuthMaster class.
import Model

-- Compatibility for old versions of Yesod
#if !MIN_VERSION_yesod(1,6,0)
type SubHandlerFor b m a = HandlerT b (HandlerT m IO) a
liftHandler :: (Monad m, MonadTrans t) => m a -> t m a
liftHandler = lift
#endif

-- | This is a cheapo synonym. Eventually AuthUser should be part of the
-- 'AuthMaster' interface, for this module to be usable outside of
-- Snowdrift.
type AuthUser = Entity User

-- | Any site that uses this subsite needs to instantiate this class.
class AuthMaster master where

    -- | Where to go after login
    postLoginRoute :: master -> Route master

    -- | Where to go after logout
    postLogoutRoute :: master -> Route master

    -- | What to show on the login page. This page should have a form that posts
    -- 'Credentials' to 'Route LoginR'. See 'AuthHarness' in the tests for a
    -- simplistic example.
    loginHandler :: HandlerT master IO Html

    -- | What to show on the create-account page. This page should post
    -- 'Credentials' to 'AuthSiteDataTypes.CreateAccountR'.
    createAccountHandler :: HandlerT master IO Html

    -- | What to show on the reset-passphrase page. This page should post
    -- 'Credentials' to 'ResetPassphraseR'
    resetPassphraseHandler :: HandlerT master IO Html

    -- | What to show on the verify-account page. This page should post
    -- 'Text' (the token) to 'VerifyAccountR'
    verifyAccountHandler :: HandlerT master IO Html

    -- | This module sends emails, in case that wasn't obvious.
    -- "Network.Mail.Mime" or "Network.Mail.Mime.SES" have good options for
    -- this method.
    sendAuthEmail :: AuthEmail -> AuthMailMessage -> HandlerT master IO ()

-- | A token used to confirm an email address.
newtype AuthToken = AuthToken { fromAuthToken :: Text } deriving Show

-- | The type of message you are expected to send.
data AuthMailMessage
        = VerifyUserCreation AuthToken
        -- ^ A user is signing up
        | VerifyPassReset AuthToken
        -- ^ A user wants to reset their passphrase
        | BadUserCreation
        -- ^ A user tried to sign up with an existing address
        | BadPassReset
        -- ^ A user tried to reset with an address that doesn't exist
        deriving Show

-- | Sanity-preserving type
newtype AuthEmail = AuthEmail { fromAuth :: Text } deriving Show

-- | Sanity-preserving type
newtype ClearPassphrase = ClearPassphrase { fromClear :: Text } deriving Show

newtype PassphraseDigest = PassphraseDigest ByteString deriving Show

-- | The "email and passphrase" representing the main purpose of this
-- module.
data Credentials = Credentials
        { credsIdent :: AuthEmail
        , credsPass :: ClearPassphrase
        } deriving (Show)

-- | Internal
data VerifiedUser = VerifiedUser
        { verifiedEmail :: Text
        , verifiedDigest :: ByteString
        }

-- | Internal
data Verification = Verification
        { verifyEmail :: AuthEmail
        , verifyToken :: AuthToken
        } deriving Show

-- | Used with Yesod caching feature
newtype CachedAuth a = CachedAuth { unCachedAuth :: Maybe a } deriving Typeable

instance (Yesod master
         ,YesodPersist master
         ,YesodPersistBackend master ~ SqlBackend
         ,RenderMessage master FormMessage
         ,AuthMaster master)
#if MIN_VERSION_yesod(1,6,0)
        => YesodSubDispatch AuthSite master where
#else
        => YesodSubDispatch AuthSite (HandlerT master IO) where
#endif
    yesodSubDispatch = $(mkYesodSubDispatch resourcesAuthSite)

-- | If the user is authenticated, get the corresponding Entity.
maybeAuth :: (YesodPersist m
             ,YesodPersistBackend m ~ SqlBackend)
          => HandlerT m IO (Maybe AuthUser)
maybeAuth = runMaybeT $ do
    k <- MaybeT $ lookupSession authSessionKey
    uid <- MaybeT $ pure (fromPathPiece k)
    u <- MaybeT $ fmap unCachedAuth (cached (runDB $ fmap CachedAuth (get uid)))
    pure (Entity uid u)

-- | If the user is /not/ authenticated, this will cause a redirect to your
-- site's 'authRoute' or simply return 'notAuthenticated'.
--
-- Note that "Yesod.Auth" is smarter about not redirecting during an API
-- request, but we don't support that yet.
requireAuth :: (Yesod m
               ,YesodPersist m
               ,YesodPersistBackend m ~ SqlBackend)
            => HandlerT m IO AuthUser
requireAuth = maybe noAuth pure =<< maybeAuth
  where
    noAuth = do
        setUltDestCurrent
        maybe notAuthenticated redirect . authRoute =<< getYesod

-- | A decent default form for 'Credentials'.
credentialsForm :: (RenderMessage (HandlerSite m) FormMessage, MonadHandler m)
                => AForm m Credentials
credentialsForm = Credentials
    <$> (AuthEmail <$>
            areq textField "Email"{fsAttrs=emailAttrs}  Nothing)
    <*> (ClearPassphrase <$>
            areq
                passwordField
                "Passphrase"{fsAttrs=ppAttrs}
                Nothing)
  where
    emailAttrs = [("autofocus",""), ("autocomplete","email")]
    ppAttrs = [("minlength","9")]

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

-- | Our wrap over makePassword
makeAuthPass :: Text -> IO ByteString
makeAuthPass t = makePassword (encodeUtf8 t) pbkdf1Strength

-- | Compare some Credentials to what's stored in the database.
checkCredentials :: MonadIO m => Credentials -> SqlPersistT m (Maybe AuthUser)
checkCredentials Credentials{..} = do
    mu <- getBy (UniqueUsr (fromAuth credsIdent))
    pure $ verify =<< mu
  where
    verify x@(Entity _ u) =
        if verifyPassword (encodeUtf8 (fromClear credsPass)) (u^.userDigest)
            then Just x
            else Nothing

-- | Verify a token given by the user. This is *destructive*; a token can
-- only ever be checked once.
checkDestroyToken :: MonadIO m => AuthToken -> SqlPersistT m (Maybe VerifiedUser)
checkDestroyToken (AuthToken t) = runMaybeT $ do
    Entity pid ProvisionalUser{..} <- MaybeT $ getBy (UniqueToken $ T.strip t)
    _ <- lift (delete pid)
    now <- liftIO getCurrentTime
    if addUTCTime timeout provisionalUserCreationTime > now
        then just (VerifiedUser provisionalUserEmail provisionalUserDigest)
        else nothing
  where
    timeout = 2 * 60 * 60 -- Two hours

-- | Generate a provisional user
provisional :: Credentials -> Verification -> IO ProvisionalUser
provisional Credentials{..} Verification{..} =
    ProvisionalUser <$> email <*> passDigest <*> token <*> curtime
  where
    email = pure (fromAuth credsIdent)
    passDigest = makeAuthPass (fromClear credsPass)
    token = pure (fromAuthToken verifyToken)
    curtime = getCurrentTime

genVerificationToken :: Credentials -> IO Verification
genVerificationToken Credentials{..} =
    Verification credsIdent . AuthToken <$> nonce128urlT tokenGenerator

-- | Create a new user for testing or development purposes. Does nothing if a
-- user with the given email already exists.
privilegedCreateUserBypass :: MonadIO m
                           => AuthEmail -> ClearPassphrase -> SqlPersistT m ()
privilegedCreateUserBypass e p = do
    now <- liftIO getCurrentTime
    passDigest <- liftIO $ makeAuthPass $ fromClear p
    void $ insertUnique $ User (fromAuth e) passDigest now now

-- | This privileged function must be used with care. It modifies the
-- user's session; it's the difference between being logged in and not!
privilegedLogin :: Yesod master => AuthUser -> HandlerT master IO ()
privilegedLogin = setSession authSessionKey . toPathPiece . entityKey

-- | Store a provisional user for later verification. Returns the token to
-- use for verification. Replaces existing token for the same email address.
-- There should be a minimum time that can pass before replacing the existing
-- token, though. Hm.
privilegedProvisionalUser :: MonadIO m
                          => Credentials -> SqlPersistT m Verification
privilegedProvisionalUser creds = do
    verf <- liftIO (genVerificationToken creds)
    prov <- liftIO (provisional creds verf)
    let constraint = UniqueProvisionalUser (provisionalUserEmail prov)
    _ <- deleteBy constraint
    verf <$ insert_ prov

-- | Log out by deleting the session var
logout :: Yesod master => HandlerT master IO ()
logout = deleteSession authSessionKey

getLoginR :: (Yesod m, RenderMessage m FormMessage, AuthMaster m)
#if MIN_VERSION_yesod(1,6,0)
          => SubHandlerFor AuthSite m Html
#else
          => HandlerT AuthSite (HandlerT m IO) Html
#endif
getLoginR = liftHandler loginHandler

postLoginR :: (Yesod master
              ,AuthMaster master
              ,YesodPersist master
              ,YesodPersistBackend master ~ SqlBackend
              ,RenderMessage master FormMessage)
           => SubHandlerFor AuthSite master Html
postLoginR = do
    ((res, _), _) <- liftHandler $ runFormPost (renderDivs credentialsForm)
    p <- getRouteToParent
    formResult (liftHandler . (runAuthResult p <=< (runDB . checkCredentials)))
               res
  where
    runAuthResult master = maybe
        (do
            render <- getUrlRenderParams
            -- the alert below is temporarily hardcoded for the
            -- Snowdrift.coop situation and should be removed for general
            -- use and after some time has gone by with this alert live.
            alertWarning ([hamlet|
                Note: if you signed up before 2017, you may need
                to <a href=@{master ResetPassphraseR}>reset your passphrase</a>.
                Also, we now require log-in IDs to be emails. If you had
                used a non-email ID, try logging in with the email
                associated with the account. If you had a non-email
                ID and no associated email, you will need to
                <a href=@{master CreateAccountR}>create a new account</a>.
                <p>
                    If you need help, or have feedback, please
                    <a href="/contact">contact&nbsp;us</a>.|] render)
            alertDanger ([hamlet|
                Sorry, the email and passphrase combination you entered was
                not recognized.|] render)
            redirect (master LoginR))
        (\u -> do
            privilegedLogin u
            alertInfo "Welcome"
            redirectUltDest =<< (postLoginRoute <$> getYesod))

#if MIN_VERSION_yesod(1,6,0)
formResult :: (Yesod yesod, RenderMessage yesod FormMessage)
           => (a -> SubHandlerFor AuthSite yesod Html)
           -> FormResult a
           -> SubHandlerFor AuthSite yesod Html
#else
formResult :: (Yesod master
              ,MonadTrans child)
           => (t -> child (HandlerT master IO) Html)
           -> FormResult t
           -> child (HandlerT master IO) Html
#endif
formResult success = \case
    FormSuccess x -> success x
    FormFailure msgs -> failure msgs
    FormMissing -> failure ["No login data" :: Text]
  where
    failure msgs =
        liftHandler $ defaultLayout [whamlet|
            <p>Auth form failures are not handled yet.
            <p>TBD: <a href="https://tree.taiga.io/project/snowdrift/issue/455">See Taiga #455</a>.
            <p>Errors: #{show msgs}
            |]

-- ** Logout page

getLogoutR :: (Yesod master
              ,AuthMaster master)
            -- => SubHandlerFor AuthSite master Html
            => SubHandlerFor AuthSite master Html
getLogoutR = liftHandler $ do
    logout
    alertInfo "You are now logged out."
    redirect =<< (postLogoutRoute <$> getYesod)

-- ** CreateAccount page

getCreateAccountR :: (Yesod yesod, RenderMessage yesod FormMessage, AuthMaster yesod)
                  => SubHandlerFor AuthSite yesod Html
getCreateAccountR = liftHandler createAccountHandler

postCreateAccountR :: (Yesod yesod
                      ,AuthMaster yesod
                      ,YesodPersistBackend yesod ~ SqlBackend
                      ,YesodPersist yesod
                      ,RenderMessage yesod FormMessage)
                   => SubHandlerFor AuthSite yesod Html
postCreateAccountR = do
    ((res, _), _) <- liftHandler $ runFormPost (renderDivs credentialsForm)
    formResult
        (\c -> do
            unless
                (validPassphrase (credsPass c))
                (passphraseError (credsIdent c))
            mu <- liftHandler (runDB (getBy (UniqueUsr (fromAuth (credsIdent c)))))
            liftHandler $ sendAuthEmail (credsIdent c) =<< maybe
                (VerifyUserCreation . verifyToken
                    <$> runDB (privilegedProvisionalUser c))
                (pure . const BadUserCreation)
                mu
            redirectParent' VerifyAccountR)
        res

-- | ResetPassphrase page
getResetPassphraseR :: (AuthMaster yesod
                       ,YesodPersist yesod
                       ,YesodPersistBackend yesod ~ SqlBackend)
                    => SubHandlerFor AuthSite yesod Html
getResetPassphraseR = liftHandler $ do
    r <- postLoginRoute <$> getYesod
    maybeAuth >>= maybe resetPassphraseHandler (const (redirect r))

postResetPassphraseR :: (Yesod yesod
                        ,AuthMaster yesod
                        ,YesodPersistBackend yesod ~ SqlBackend
                        ,YesodPersist yesod
                        ,RenderMessage yesod FormMessage)
                     => SubHandlerFor AuthSite yesod Html
postResetPassphraseR = do
    ((res, _), _) <- liftHandler $ runFormPost (renderDivs credentialsForm)
    formResult
        (\c -> do
            mu <- liftHandler (runDB (getBy (UniqueUsr (fromAuth (credsIdent c)))))
            liftHandler $ sendAuthEmail (credsIdent c) =<< maybe
                (pure BadPassReset)
                (const $ VerifyPassReset . verifyToken
                    <$> runDB (privilegedProvisionalUser c))
                mu
            redirectParent' VerifyAccountR)
        res

-- | This should become part of the AuthSite interface
validPassphrase :: ClearPassphrase -> Bool
validPassphrase = (>= 9) . T.length . fromClear

-- | What to do on passphrase error. Should also be part of the AuthSite
-- interface.
passphraseError
    :: Yesod yesod
    => AuthEmail
    -> SubHandlerFor AuthSite yesod b
passphraseError e = do
    addMessage
        "warning"
        "Sorry, passphrase must be at least nine characters. Please try again."
    redirectParent CreateAccountR [("auth", fromAuth e)]

-- | VerifyAccount page
--
-- For easy links that do things without javascript, this GET handler will
-- do the same thing as the POST if the query param `v=$TOKEN` is present.
getVerifyAccountR :: (Yesod yesod
                     ,RenderMessage yesod FormMessage
                     ,AuthMaster yesod
                     ,YesodPersist yesod
                     ,YesodPersistBackend yesod ~ SqlBackend)
                  => SubHandlerFor AuthSite yesod Html
getVerifyAccountR = do
    mtoken <- liftHandler $ runInputGet $ fmap AuthToken <$> iopt textField "v"
    maybe (liftHandler verifyAccountHandler) runToken mtoken

-- | Handle an attempted verification.
--
-- This method is rather blithe in the belief that the privileged methods
-- above ensure that a good token is truly "good".
postVerifyAccountR :: (Yesod yesod
                      ,YesodPersist yesod
                      ,YesodPersistBackend yesod ~ SqlBackend
                      ,RenderMessage yesod FormMessage)
                   => SubHandlerFor AuthSite yesod Html
postVerifyAccountR = do
    ((res, _), _) <-
        liftHandler $ runFormPost (renderDivs (AuthToken <$> areq textField "" Nothing))
    formResult runToken res

runToken :: (Yesod yesod
            ,YesodPersist yesod
            ,YesodPersistBackend yesod ~ SqlBackend)
         => AuthToken -> SubHandlerFor AuthSite yesod Html
runToken tok = do
    -- Have to check the token and insert the user in the same
    -- transaction, lest race conditions boggle the contraptions
    m <- liftHandler $ runDB $ traverse upsertUser =<< checkDestroyToken tok
    case m of
        Nothing -> do
            -- For https://tree.taiga.io/project/snowdrift/task/405, we
            -- may want to modify this as well (or make it more
            -- accessible to front end devs).
            liftHandler $ alertWarning "Uh oh, your token appears to be invalid!"
            redirectParent' VerifyAccountR
        Just _ -> do
            liftHandler $ alertSuccess "You are all set! Log in to continue."
            redirectParent' LoginR
  where
    upsertUser :: MonadIO m => VerifiedUser -> SqlPersistT m (Entity User)
    upsertUser VerifiedUser{..} = do
        now <- liftIO getCurrentTime
        upsert (User verifiedEmail verifiedDigest now now)
               [UserDigest =. verifiedDigest, UserPassUpdated =. now]

redirectParent' :: Route child -> SubHandlerFor child yesod b
redirectParent' route  = redirectParent route []

redirectParent
    :: Route child
    -> [(Text, Text)]
    -> SubHandlerFor child yesod b
redirectParent r qs = do
    p <- getRouteToParent
    liftHandler (redirect (p r, qs))
