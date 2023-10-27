{-# OPTIONS_HADDOCK hide, prune #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}

#if MIN_VERSION_base(4,10,0)
{-# LANGUAGE DerivingStrategies #-}
#endif

-- | Core definitions for the Auth subsite (see AuthSite.hs)
module AuthSiteDataTypes where

import ClassyPrelude

import Database.Persist.TH
import Yesod.Core

-- | Foundation type for the subsite.
data AuthSite = AuthSite

mkYesodSubData "AuthSite" [parseRoutes|
/login LoginR GET POST
/logout LogoutR GET
/create-account CreateAccountR GET POST
/verify-account VerifyAccountR GET POST
/reset-passphrase ResetPassphraseR GET POST
|]

share [mkPersist sqlSettings
      , mkMigrate "migrateAuthSite"
      ] [persistLowerCase|
ProvisionalUser sql="authsite__provisional_user"
    email Text
    digest ByteString
    token Text
    creationTime UTCTime

    UniqueProvisionalUser email
    UniqueToken token

    deriving Show
|]
