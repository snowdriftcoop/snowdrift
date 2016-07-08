-- | Core definitions for the Auth subsite (see AuthSite.hs)
module AuthSiteTypes where

import ClassyPrelude

import Database.Persist.TH
import Yesod.Core

-- | The loginDest parameter is the route that users should be directed to
-- after logging in.
data AuthSite = AuthSite

mkYesodSubData "AuthSite" [parseRoutes|
/login LoginR GET POST
/logout LogoutR POST
/create-account CreateAccountR GET POST
/verify-account VerifyAccountR GET POST
/reset-passphrase ResetPassphraseR GET POST
|]

share [mkPersist sqlSettings{mpsPrefixFields = False}
      , mkMigrate "migrateAuth"
      ] [persistLowerCase|
ProvisionalUser
    puEmail Text           sql="email"
    puDigest ByteString    sql="digest"
    puToken Text           sql="token"
    puCreationTime UTCTime sql="creation_time"

    UniqueProvUsr puEmail
    UniqueTok puToken

    deriving Show
|]
