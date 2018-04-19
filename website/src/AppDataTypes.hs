{-# LANGUAGE RankNTypes #-}

-- | Core Template Haskell-generated types, such as the site's routes.
--
-- This file should be named "SiteTypes", and App should be named Site,
-- but I'm in the middle of something else so it will have to be done
-- later.
module AppDataTypes where

import ClassyPrelude.Yesod

import Database.Persist.Sql (ConnectionPool)
import Web.Stripe
import Yesod.Core.Types (Logger)
import Yesod.GitRev

import Crowdmatch (StripeActions)

import AuthSite
import Settings

-- | The God-object available to every Handler. This is the site's
-- foundation ("yesod").
data App = App
    { appSettings      :: AppSettings
    , appStatic        :: Static -- ^ Settings for static file serving.
    , appConnPool      :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager   :: Manager
    , appLogger        :: Logger
    , appGitRev        :: GitRev
    , appAuth          :: AuthSite
    , appStripeActions :: StripeActions
    }

-- This function generates the route types, and also generates the
-- following type synonyms:
-- type Handler = HandlerT App IO
-- type Widget = WidgetT App IO ()
mkYesodData "App" $(parseRoutesFile "config/routes")
