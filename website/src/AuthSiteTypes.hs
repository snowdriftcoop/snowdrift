-- | Core definitions for the Auth subsite (see AuthSite.hs)
module AuthSiteTypes where

import ClassyPrelude

import Database.Persist.TH
import Yesod.Core

-- | The loginDest parameter is the route that users should be directed to
-- after logging in.
data AuthSite loginDest = AuthSite loginDest

mkYesodSubData "AuthSite" [parseRoutes|
/login LoginR GET POST
/logout LogoutR POST
|]

share [mkPersist sqlSettings{mpsPrefixFields = False}
      , mkMigrate "migrateAuth"
      ] [persistLowerCase|
ProvisionalUser
    provisionalEmail Text
    provisionalDigest ByteString
    verificationDigest ByteString
    puCreationTime UTCTime
|]
