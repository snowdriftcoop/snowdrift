{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Subsite for email-and-passphrase authentication.
module AuthSite (module AuthSiteTypes, module AuthSite) where

import Prelude

import Data.Text (Text)
import Database.Persist
import Yesod

import AuthSiteTypes
import Css
import Model
import Settings

-- To create a usable API, AuthUser would have to be added to an exposed
-- typeclass.
type AuthUser = Entity User

instance (Yesod master, RenderMessage master FormMessage)
    => YesodSubDispatch AuthSite (HandlerT master IO) where
    yesodSubDispatch = $(mkYesodSubDispatch resourcesAuthSite)

-- ## Duplicating Yesod.Auth API for a wee while.
getAuth :: a -> AuthSite
getAuth _ = AuthSite

maybeAuth :: HandlerT m IO (Maybe AuthUser)
maybeAuth = pure Nothing

requireAuth :: HandlerT m IO AuthUser
requireAuth = pure undefined

newtype AuthEmail = AuthEmail Text deriving Show
newtype ClearPassphrase = ClearPassphrase Text deriving Show

data LoginD = LoginD AuthEmail ClearPassphrase deriving (Show)

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

postLoginR :: (Yesod master, RenderMessage master FormMessage)
           => HandlerT AuthSite (HandlerT master IO) Html
postLoginR = lift $ do
    ((res, _), _) <- runFormPost (renderDivs loginForm)
    let x = show res
    defaultLayout [whamlet|X is #{x}|]

    -- u <- getBy (UniqueEmail

postLogoutR :: Yesod master => HandlerT AuthSite (HandlerT master IO) Html
postLogoutR = undefined
