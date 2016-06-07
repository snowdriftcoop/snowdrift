{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Subsite for email-and-passphrase authentication.
module AuthSite (module Auth, module AuthSite) where

import Prelude
import Model
import Database.Persist

import Yesod.Core

import Auth

instance Yesod master => YesodSubDispatch Auth (HandlerT master IO) where
    yesodSubDispatch = $(mkYesodSubDispatch resourcesAuth)

type AuthRoute = Route Auth

type AuthHandler master a = HandlerT Auth (HandlerT master IO) a

getAuth :: a -> Auth
getAuth _ = Auth

data Pep = Pep

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
