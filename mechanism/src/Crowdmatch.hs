{-# LANGUAGE GADTs #-}
module Crowdmatch (module Crowdmatch) where

import Control.Lens
import Control.Monad (void)
import Control.Monad.IO.Class
import Data.List (intersperse)
import Data.Text (Text)
import Data.Time
import Database.Persist
import Database.Persist.Sql
import Web.Stripe.Customer (Customer(..), CustomerId(..))

import Crowdmatch.Model as Crowdmatch hiding (Patron(..))
import qualified Crowdmatch.Model as Model
import qualified Crowdmatch.Skeleton as Skeleton

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

newtype PaymentToken = PaymentToken Text deriving (Show)

storeStripeCustomer
    :: (ToMechPatron u, MonadIO m)
    => u -> PaymentToken -> SqlPersistT m ()
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

fetchCrowdCount
    :: MonadIO m
    => SqlPersistT m Crowd
fetchCrowdCount = runMech ActFetchCrowdCount

fetchPatron
    :: (ToMechPatron u, MonadIO m)
    => u -> SqlPersistT m Patron
fetchPatron = runMech . ActFetchPatron . (^. from external)

--
-- ONE LEVEL DOWN
-- wherein we use our internal, Markov-able api
--
data MechAction b where
    ActStoreStripeCustomer :: PPtr -> PaymentToken -> MechAction ()
    ActDeleteStripeCustomer :: PPtr -> MechAction ()
    ActStorePledge :: PPtr -> MechAction ()
    ActDeletePledge :: PPtr -> MechAction ()
    ActFetchCrowdCount :: MechAction Crowd
    ActFetchPatron :: PPtr -> MechAction Patron

instance Show (MechAction b) where
    show (ActStoreStripeCustomer p c) = show2 "ActStoreStripeCustomer " p c
    show (ActDeleteStripeCustomer p) = show1 "ActDeleteStripeCustomer" p
    show (ActStorePledge p) = show1 "ActStorePledge" p
    show (ActDeletePledge p) = show1 "ActDeletePledge" p
    show ActFetchCrowdCount = "ActFetchCrowdCount"
    show (ActFetchPatron p) = show1 "ActFetchPatron" p

show1 :: Show a => String -> a -> String
show1 str a = str ++ " " ++ show a

show2 :: (Show a, Show b) => String -> a -> b -> String
show2 str a b = unwords (intersperse " " [str, show a, show b])

runMech :: MonadIO m => MechAction b -> SqlPersistT m b

-- FIXME: Feedback on Stripe error
runMech (ActStoreStripeCustomer pptr cust) = do
    Entity pid p <- upsertPatron pptr []
    ret <- maybe create' update' (Model.patronStripeCustomer p)
    either (const (pure ())) (updatePatron' pid) ret
  where
    create' = error "Stripe create Customer"
    update' (CustomerId c) = error "Stripe update Customer"
    updatePatron' pid c = update pid [PatronStripeCustomer =. Just (customerId c)]

-- FIXME: Feedback on Stripe error or nonexisting CustomerId.
runMech (ActDeleteStripeCustomer pptr) = do
    Entity pid p <- upsertPatron pptr []
    maybe (pure ()) blop (Model.patronStripeCustomer p)
    maybe (pure ()) (const (dropPledge' pid)) (Model.patronPledgeSince p)
  where
    blop = error "Stripe delete Customer"
    dropPledge' pid = do
        now <- liftIO getCurrentTime
        update pid [PatronPledgeSince =. Nothing]
        insert_ (PledgeHistory pid now DeletePledge)

-- WEIRD: No feedback on existing pledges. Need that. But if I add it here,
-- I break the ability to do Markov crap. Should I further break it into
-- input-only and output-only? Hmmm
runMech (ActStorePledge pptr) = do
    Entity pid p <- upsertPatron pptr []
    maybe (pure ()) (const (pledge' pid)) (Model.patronStripeCustomer p)
  where
    pledge' pid = do
        now <- liftIO getCurrentTime
        void (update pid [PatronPledgeSince =. Just now])

-- FIXME: Feedback on nonexistent pledge.
runMech (ActDeletePledge pptr) =
    void (upsertPatron pptr [PatronPledgeSince =. Nothing])
runMech ActFetchCrowdCount = Crowd <$> Skeleton.countActivePatrons
runMech (ActFetchPatron pptr) =
    -- Haskell doesn't know it, but a PPtr should always be linked to a
    -- Patron. This function ensures it in Haskell-land.
    --
    -- (Also, creating the proper database constraint is still TODO, so we
    -- actually need this code.)
    fromModel . entityVal <$> upsertPatron pptr []

upsertPatron pptr mods = do
    now <- liftIO getCurrentTime
    upsert (Model.Patron pptr now Nothing 0 Nothing) mods

fromModel :: Model.Patron -> Patron
fromModel (Model.Patron u t c d p) = Patron t c d p

toModel :: ToMechPatron p => p -> Patron -> Model.Patron
toModel u (Patron t c d p) = Model.Patron (u ^. from external) t c d p
