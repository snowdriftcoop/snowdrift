{-# LANGUAGE FlexibleInstances #-}

module Model (module Model) where

import ClassyPrelude.Yesod
import Database.Persist.Quasi
import Web.Stripe.Customer (CustomerId(..))

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [ mkPersist sqlSettings{ mpsGenerateLenses = True }
      , mkMigrate "migrateSnowdrift"]
      $(persistFileWith lowerCaseSettings "config/models")


-- | Haskell doesn't know it, but a User should always be linked to a
-- Patron. This function ensures it in Haskell-land.
--
-- (Also, creating the proper database constraint is still TODO, so we
-- actually need this code.)
fetchUserPatron :: MonadIO m => UserId -> SqlPersistT m (Entity Patron)
fetchUserPatron uid = do
    now <- liftIO getCurrentTime
    patron <- upsert (Patron uid now Nothing 0 Nothing) []
    update uid [UserPatron =. Just (entityKey patron)]
    pure patron
