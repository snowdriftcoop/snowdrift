{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}

#if MIN_VERSION_base(4,10,0)
{-# LANGUAGE DerivingStrategies #-}
#endif

module Model (module Model) where

import ClassyPrelude.Yesod
import Database.Persist.Quasi
import Database.Persist.Sql
import Crowdmatch (ToCrowdmatchPatron(..))

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [ mkPersist sqlSettings{ mpsGenerateLenses = True }
      , mkMigrate "migrateSnowdrift"]
      $(persistFileWith lowerCaseSettings "config/models")

instance ToCrowdmatchPatron UserId where
    toMechPatron = fromIntegral . unSqlBackendKey . unUserKey
    fromMechPatron = UserKey . SqlBackendKey . fromIntegral
