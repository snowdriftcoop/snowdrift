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
import Data.Maybe
import Data.Text (Text)
import Data.Text.Encoding
import Database.Persist.Sql
import Yesod
import qualified Data.Text as T

import Alerts
import AuthSiteTypes
import Css
import Model
import Settings

-- To create a usable API, AuthUser would have to be added to an exposed
-- typeclass.
type AuthUser = Entity User

-- ## Internal types

newtype AuthEmail = AuthEmail Text deriving Show
makePrisms ''AuthEmail

newtype ClearPassphrase = ClearPassphrase Text deriving Show
makePrisms ''ClearPassphrase

data LoginD = LoginD
        { _loginAuth :: AuthEmail
        , _loginPass :: ClearPassphrase
        } deriving (Show)
makeLenses ''LoginD

instance (Yesod master
         ,YesodPersist master
         ,YesodPersistBackend master ~ SqlBackend
         ,RedirectUrl master r
         ,RenderMessage master FormMessage)
    => YesodSubDispatch (AuthSite r) (HandlerT master IO) where
    yesodSubDispatch = $(mkYesodSubDispatch resourcesAuthSite)

-- ## Duplicating Yesod.Auth API for a wee while.

maybeAuth :: HandlerT m IO (Maybe AuthUser)
maybeAuth = pure Nothing

requireAuth :: HandlerT m IO AuthUser
requireAuth = pure undefined

-- ## Now building the login page.

loginForm :: (RenderMessage (HandlerSite m) FormMessage, MonadHandler m)
          => AForm m LoginD
loginForm = LoginD
    <$> (AuthEmail <$> areq textField "Email"{fsAttrs=emailAttrs}  Nothing)
    <*> (ClearPassphrase <$> areq passwordField "Passphrase" Nothing)
  where
    emailAttrs = [("autofocus",""), ("autocomplete","email")]

getLoginR :: (Yesod master, RenderMessage master FormMessage)
          => HandlerT (AuthSite r) (HandlerT master IO) Html
getLoginR = lift $ do
    ((_, loginFields), enctype) <- runFormPost (renderDivs loginForm)
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
    runAuthResult home parent = \case
        Authenticated -> do
            alertInfo "Welcome"
            redirect home
        BadCredentials -> do
            alertDanger [shamlet|Bad credentials:  <a href="https://tree.taiga.io/project/snowdrift/us/392">See Taiga #392</a>.|]
            redirect (parent LoginR)
    loginAgain msgs = do
        lift $ defaultLayout [whamlet|
            <p>Login form failures are not handled yet.
            <p>TBD: <a href="https://tree.taiga.io/project/snowdrift/us/392">See Taiga #392</a>.
            <p>Errors: #{show msgs}
            |]

    formResult success fail = \case
        FormSuccess x -> success x
        FormFailure msgs -> fail msgs
        FormMissing -> fail ["No login data"]

verify :: ClearPassphrase -> ByteString -> Bool
verify (ClearPassphrase pass) hash = verifyPassword (encodeUtf8 pass) hash

data Authenticated = Authenticated | BadCredentials deriving Show

checkCredentials creds = do
    u <- getBy $ UniqueUsr (creds^.loginAuth._AuthEmail)
    pure $ maybe BadCredentials (goVerify . entityVal) u
  where
    goVerify u = if verify (creds^.loginPass) (u^.userPassphrase)
        then Authenticated
        else BadCredentials

postLogoutR :: Yesod master => HandlerT (AuthSite r) (HandlerT master IO) Html
postLogoutR = undefined
