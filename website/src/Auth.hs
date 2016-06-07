-- | Core definitions for the Auth subsite (see AuthSite.hs)
module Auth where

import Yesod.Core

data Auth = Auth

mkYesodSubData "Auth" [parseRoutes|
/login LoginR GET POST
/logout LogoutR POST
|]
