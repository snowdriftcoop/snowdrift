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
import Database.Persist
import Yesod
import qualified Data.Text as T

import AuthSiteTypes
import Css
import Model
import Settings

-- To create a usable API, AuthUser would have to be added to an exposed
-- typeclass.
type AuthUser = Entity User


-- ## Login needs

newtype AuthEmail = AuthEmail Text deriving Show
makePrisms ''AuthEmail

newtype ClearPassphrase = ClearPassphrase { _passText :: Text } deriving Show
makePrisms ''ClearPassphrase

data LoginD = LoginD
        { _loginAuth :: AuthEmail
        , _loginPass :: ClearPassphrase
        } deriving (Show)
makeLenses ''LoginD

instance (Yesod master, YesodPersist master, RenderMessage master FormMessage)
    => YesodSubDispatch AuthSite (HandlerT master IO) where
    yesodSubDispatch = $(mkYesodSubDispatch resourcesAuthSite)

-- ## Duplicating Yesod.Auth API for a wee while.

getAuth :: a -> AuthSite
getAuth _ = AuthSite

maybeAuth :: HandlerT m IO (Maybe AuthUser)
maybeAuth = pure Nothing

requireAuth :: HandlerT m IO AuthUser
requireAuth = pure undefined


-- ## Now building the login page

loginForm = LoginD
    <$> (AuthEmail <$> areq textField "Email" Nothing)
    <*> (ClearPassphrase <$> areq passwordField "Passphrase" Nothing)

getLoginR :: (Yesod master, RenderMessage master FormMessage)
          => HandlerT AuthSite (HandlerT master IO) Html
getLoginR = lift $ do
    ((_, loginFields), enctype) <- runFormPost (renderDivs loginForm)
    defaultLayout $ do
        setTitle "Login — Snowdrift.coop"
        $(widgetFile "page/auth/login")

postLoginR :: (Yesod master, YesodPersist master, RenderMessage master FormMessage)
           => HandlerT AuthSite (HandlerT master IO) Html
postLoginR = do
    ((res', _), _) <- lift $ runFormPost (renderDivs loginForm)
    formResult (lift . (runAuthResult <=< (runDB . checkCredentials)))
               loginAgain
               res'
  where
    runAuthResult _ = defaultLayout [whamlet|It has happened|]
    loginAgain msgs = do
        lift $ defaultLayout [whamlet|
            <p>Login form failures are not handled yet.
            <p>TBD: <a href="https://tree.taiga.io/project/snowdrift/us/392">See Taiga #392</a>.
            <p>Errors: #{show msgs}
            |]

    formResult success fail = \case
        FormSuccess x -> success x
        FormFailure msgs -> fail msgs
        FormMissing -> redirect LoginR

verify :: ClearPassphrase -> ByteString -> Bool
verify (ClearPassphrase pass) hash = verifyPassword (encodeUtf8 pass) hash

checkCredentials creds = do
    u <- getBy $ UniqueUsr (creds^.loginAuth._AuthEmail)
    pure ()
    -- lift $ runDB 
    -- let x = show creds
    -- lift $ defaultLayout [whamlet|X is #{x}|]

postLogoutR :: Yesod master => HandlerT AuthSite (HandlerT master IO) Html
postLogoutR = undefined
