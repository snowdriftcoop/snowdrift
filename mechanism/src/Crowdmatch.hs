{-# LANGUAGE GADTs #-}
module Crowdmatch (module Crowdmatch) where

import Control.Lens
import Control.Monad.IO.Class
import Data.List (intersperse)
import Data.Time
import Database.Persist
import Database.Persist.Sql
import Web.Stripe.Customer (CustomerId(..))

import Crowdmatch.Model as Crowdmatch hiding (Patron(..))
import qualified Crowdmatch.Model as Model

--
-- THE ACTUAL INTERFACE USED BY THE WEBSITE
--

data Patron = Patron
        { patronCreated :: UTCTime
        , patronStripeCustomer :: Maybe CustomerId
        , patronDonationPayable :: DonationUnits
        , patronPledgeSince :: Maybe UTCTime
        }

data Crowd = Crowd { crowdSize :: Int }

storeStripeCustomer
    :: (ToMechPatron u, MonadIO m)
    => u -> CustomerId -> SqlPersistT m ()
storeStripeCustomer u = runMech . ActStoreStripeCustomer (u ^. from external)

deleteStripeCustomer
    :: (ToMechPatron u, MonadIO m)
    => u -> SqlPersistT m ()
deleteStripeCustomer = runMech . ActDeleteStripeCustomer . (^. from external)

storePledge
    :: (ToMechPatron u, MonadIO m)
    => u -> SqlPersistT m ()
storePledge = runMech . ActStorePledge . (^. from external)

deletePledge
    :: (ToMechPatron u, MonadIO m)
    => u -> SqlPersistT m ()
deletePledge = runMech . ActDeletePledge . (^. from external)

fetchCrowd
    :: MonadIO m
    => SqlPersistT m Crowd
fetchCrowd = runMech ActFetchCrowd

fetchPatron
    :: (ToMechPatron u, MonadIO m)
    => u -> SqlPersistT m Patron
fetchPatron = runMech . ActFetchPatron . (^. from external)

--
-- ONE LEVEL DOWN
-- wherein we use our internal, Markov-able api
--
data MechAction b where
    ActStoreStripeCustomer :: PPtr -> CustomerId -> MechAction ()
    ActDeleteStripeCustomer :: PPtr -> MechAction ()
    ActStorePledge :: PPtr -> MechAction ()
    ActDeletePledge :: PPtr -> MechAction ()
    ActFetchCrowd :: MechAction Crowd
    ActFetchPatron :: PPtr -> MechAction Patron

instance Show (MechAction b) where
    show (ActStoreStripeCustomer p c) = show2 "ActStoreStripeCustomer " p c
    show (ActDeleteStripeCustomer p) = show1 "ActDeleteStripeCustomer" p
    show (ActStorePledge p) = show1 "ActStorePledge" p
    show (ActDeletePledge p) = show1 "ActDeletePledge" p
    show ActFetchCrowd = "ActFetchCrowd"
    show (ActFetchPatron p) = show1 "ActFetchPatron" p

show1 :: Show a => String -> a -> String
show1 str a = str ++ " " ++ show a

show2 :: (Show a, Show b) => String -> a -> b -> String
show2 str a b = unwords (intersperse " " [str, show a, show b])

runMech :: MonadIO m => MechAction b -> SqlPersistT m b
runMech (ActStoreStripeCustomer pptr cust) = error "Nope"
runMech (ActDeleteStripeCustomer pptr) = error "Nope"
runMech (ActStorePledge pptr) = error "Nope"
runMech (ActDeletePledge pptr) = error "Nope"
runMech ActFetchCrowd = error "Nope"
runMech (ActFetchPatron pptr) = do
    now <- liftIO getCurrentTime
    -- Haskell doesn't know it, but a PPtr should always be linked to a
    -- Patron. This function ensures it in Haskell-land.
    --
    -- (Also, creating the proper database constraint is still TODO, so we
    -- actually need this code.)
    fromModel . entityVal <$>
        upsert (Model.Patron pptr now Nothing 0 Nothing) []

fromModel :: Model.Patron -> Patron
fromModel (Model.Patron u t c d p) = Patron t c d p

toModel :: ToMechPatron p => p -> Patron -> Model.Patron
toModel u (Patron t c d p) = Model.Patron (u ^. from external) t c d p
