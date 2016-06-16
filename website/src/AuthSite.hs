{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Subsite for email-and-passphrase authentication.
module AuthSite (module AuthSiteTypes, module AuthSite) where

import Prelude
import Model
import Database.Persist

import Yesod.Core

import AuthSiteTypes

instance Yesod master => YesodSubDispatch AuthSite (HandlerT master IO) where
    yesodSubDispatch = $(mkYesodSubDispatch resourcesAuthSite)

type AuthRoute = Route AuthSite

type AuthHandler master a = HandlerT AuthSite (HandlerT master IO) a

getAuth :: a -> AuthSite
getAuth _ = AuthSite

maybeAuth :: HandlerT m IO (Maybe (Entity User))
maybeAuth = pure Nothing

requireAuth :: HandlerT m IO (Entity User)
requireAuth = pure undefined

getLoginR :: AuthHandler master Html
getLoginR = undefined

postLoginR :: AuthHandler master Html
postLoginR = undefined

postLogoutR :: AuthHandler master Html
postLogoutR = undefined
