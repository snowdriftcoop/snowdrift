-- | Core definitions for the Auth subsite (see AuthSite.hs)
module AuthSiteTypes where

import Yesod.Core

-- | The loginDest parameter is the route that users should be directed to
-- after logging in.
data AuthSite loginDest = AuthSite loginDest

mkYesodSubData "AuthSite" [parseRoutes|
/login LoginR GET POST
/logout LogoutR POST
|]
