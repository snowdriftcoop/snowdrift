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
import Database.Persist.Sql
import Yesod

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

authKey :: Text
authKey = "_AUTHID"

checkCredentials :: MonadIO m => Credentials -> SqlPersistT m (Maybe AuthUser)
checkCredentials Credentials{..} = do
    mu <- getBy (UniqueUsr (fromAuth loginAuth))
    case mu of
        Just (Entity uid u) -> do
            pure $ if verifyPassword (encodeUtf8 (fromClear loginPass)) (u^.userDigest)
                then mu
                else Nothing
        Nothing -> pure Nothing

-- | This privileged function must be used with care.
privilegedCreateUser :: MonadIO m => VerifiedUser -> SqlPersistT m AuthUser
privilegedCreateUser VerifiedUser{..} = insertEntity (User verifiedEmail verifiedDigest)

-- | This privileged function must be used with care. It modifies the
-- user's session; it's the difference between being logged in and not!
priviligedLogin :: Yesod master => AuthUser -> HandlerT master IO ()
priviligedLogin = setSession authKey . toPathPiece . entityKey

-- | Log the user out.
logout :: Yesod master => HandlerT master IO ()
logout = deleteSession authKey

data Verification
verifyEmail :: MonadIO m => Verification -> SqlPersistT m (Maybe VerifiedUser)
verifyEmail = undefined

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
