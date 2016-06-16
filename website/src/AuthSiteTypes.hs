-- | Core definitions for the Auth subsite (see AuthSite.hs)
module AuthSiteTypes where

import Yesod.Core

data AuthSite = AuthSite

mkYesodSubData "AuthSite" [parseRoutes|
/login LoginR GET POST
/logout LogoutR POST
|]
