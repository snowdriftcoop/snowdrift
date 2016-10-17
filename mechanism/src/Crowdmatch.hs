{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Crowdmatch (module Crowdmatch) where

import Control.Lens
import Control.Monad (void, (<=<))
import Control.Monad.IO.Class
import Control.Monad.Operational
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

-- | Actions provided by the library
data MechAction b where
    ActStoreStripeCustomer :: PPtr -> PaymentToken -> MechAction ()
    ActDeleteStripeCustomer :: PPtr -> MechAction ()
    ActStorePledge :: PPtr -> MechAction ()
    ActDeletePledge :: PPtr -> MechAction ()
    ActFetchCrowdCount :: MechAction Crowd
    ActFetchPatron :: PPtr -> MechAction Patron

-- | Executing the actions
runMech :: MonadIO m => MechAction b -> SqlPersistT m b

-- FIXME: Feedback on Stripe error
runMech (ActStoreStripeCustomer pptr tok) = do
    Entity pid p <- upsertPatron pptr []
    ret <- maybe create' update' (Model.patronStripeCustomer p)
    either (const (pure ())) (updatePatron' pid) ret
  where
    create' = liftIO (dummyStripe (stripeCreateCustomer tok))
    update' = liftIO . dummyStripe . stripeUpdateCustomer tok
    updatePatron' pid c = update pid [PatronStripeCustomer =. Just (customerId c)]

-- FIXME: Feedback on Stripe error or nonexisting CustomerId.
runMech (ActDeleteStripeCustomer pptr) = do
    Entity pid p <- upsertPatron pptr []
    maybe (pure (Right ())) delete' (Model.patronStripeCustomer p)
    maybe (pure ()) (const (dropPledge' pid)) (Model.patronPledgeSince p)
  where
    delete' = liftIO . dummyStripe . stripeDeleteCustomer
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
    fromModel . entityVal <$> upsertPatron pptr []

--
-- I N C E P T I O N
--
-- (one more level down)
-- Wherein we abstract over the possible ways of running Stripe.
--

data StripeI a where
    CreateCustomerI :: PaymentToken -> StripeI Customer
    UpdateCustomerI :: PaymentToken -> CustomerId -> StripeI Customer
    DeleteCustomerI :: CustomerId -> StripeI ()

type StripeT m a = ProgramT StripeI m a

stripeCreateCustomer :: MonadIO m => PaymentToken -> StripeT m Customer
stripeCreateCustomer = singleton . CreateCustomerI

stripeUpdateCustomer :: MonadIO m => PaymentToken -> CustomerId -> StripeT m Customer
stripeUpdateCustomer tok = singleton . UpdateCustomerI tok

stripeDeleteCustomer :: MonadIO m => CustomerId -> StripeT m ()
stripeDeleteCustomer = singleton . DeleteCustomerI

dummyStripe :: MonadIO m => StripeT m a -> m (Either b a)
dummyStripe = eval <=< viewT
  where
    eval = \case
        Return x -> pure (Right x)
        CreateCustomerI _tok :>>= k -> dummyStripe $ k Customer{ customerId = CustomerId "dummy" }
        UpdateCustomerI _tok cust :>>= k -> dummyStripe $ k Customer{ customerId = cust }
        DeleteCustomerI _ :>>= k -> dummyStripe $ k ()

--
-- Helpers
--

-- | Haskell doesn't know it, but a PPtr should always be linked to a
-- Patron. This function ensures it in Haskell-land.
--
-- (Also, creating the proper database constraint is still TODO, so we
-- actually need this code.)
upsertPatron pptr mods = do
    now <- liftIO getCurrentTime
    upsert (Model.Patron pptr now Nothing 0 Nothing) mods

fromModel :: Model.Patron -> Patron
fromModel (Model.Patron u t c d p) = Patron t c d p

toModel :: ToMechPatron p => p -> Patron -> Model.Patron
toModel u (Patron t c d p) = Model.Patron (u ^. from external) t c d p
