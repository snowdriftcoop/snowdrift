{-# LANGUAGE RankNTypes #-}
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
import Web.Stripe
import Web.Stripe.Error
import Web.Stripe.Customer

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

type SqlRunner io env = forall a. SqlPersistT io a -> env a
type StripeRunner = forall a. StripeT IO a -> IO (Either StripeError a)

storeStripeCustomer
    :: (ToMechPatron usr, MonadIO io, MonadIO env)
    => SqlRunner io env
    -> StripeRunner
    -> usr
    -> PaymentToken
    -> env ()
storeStripeCustomer db strp usr =
    runMech db . ActStoreStripeCustomer strp (usr ^. from external)

deleteStripeCustomer
    :: (ToMechPatron usr, MonadIO io, MonadIO env)
    => SqlRunner io env
    -> StripeRunner
    -> usr
    -> env ()
deleteStripeCustomer db strp =
    runMech db . ActDeleteStripeCustomer strp . (^. from external)

storePledge
    :: (ToMechPatron usr, MonadIO io, MonadIO env)
    => SqlRunner io env
    -> usr
    -> env ()
storePledge db = runMech db . ActStorePledge . (^. from external)

deletePledge
    :: (ToMechPatron usr, MonadIO io, MonadIO env)
    => SqlRunner io env
    -> usr
    -> env ()
deletePledge db = runMech db . ActDeletePledge . (^. from external)

fetchCrowdCount
    :: (MonadIO io, MonadIO env)
    => SqlRunner io env
    -> env Crowd
fetchCrowdCount db = runMech db ActFetchCrowdCount

fetchPatron
    :: (ToMechPatron usr, MonadIO io, MonadIO env)
    => SqlRunner io env
    -> usr
    -> env Patron
fetchPatron db = runMech db . ActFetchPatron . (^. from external)

--
-- ONE LEVEL DOWN
-- wherein we use our internal, Markov-able api
--

-- | Actions provided by the library
data MechAction b where
    ActStoreStripeCustomer
        :: StripeRunner
        -> PPtr
        -> PaymentToken
        -> MechAction ()
    ActDeleteStripeCustomer :: StripeRunner -> PPtr -> MechAction ()
    ActStorePledge :: PPtr -> MechAction ()
    ActDeletePledge :: PPtr -> MechAction ()
    ActFetchCrowdCount :: MechAction Crowd
    ActFetchPatron :: PPtr -> MechAction Patron

-- | Executing the actions
runMech
    :: (MonadIO env, MonadIO io)
    => SqlRunner io env -> MechAction b -> env b

--
-- StripeCustomer (store/delete)
--

-- FIXME: Feedback on Stripe error
runMech db (ActStoreStripeCustomer strp pptr tok) = do
    Entity pid p <- db (upsertPatron pptr [])
    ret <- maybe create' update' (Model.patronStripeCustomer p)
    either (const (pure ())) (updatePatron' pid) ret
  where
    create' = liftIO (strp (stripeCreateCustomer tok))
    update' = liftIO . strp . stripeUpdateCustomer tok
    updatePatron' pid c =
        db (update pid [PatronStripeCustomer =. Just (customerId c)])

-- FIXME: Feedback on Stripe error or nonexisting CustomerId.
runMech db (ActDeleteStripeCustomer strp pptr) = do
    Entity pid p <- db (upsertPatron pptr [])
    -- Fixme: Duplication of actions
    -- Must delete pledges if there's no payment method!
    runMech db (ActDeletePledge pptr)
    maybe (pure ()) (deleteCust' pid) (Model.patronStripeCustomer p)
  where
    deleteCust' pid cust = do
        liftIO (strp (stripeDeleteCustomer cust))
        now <- liftIO getCurrentTime
        db (update pid [PatronStripeCustomer =. Nothing])

--
-- Pledge (store/delete)
--

-- FIXME: Feedback on missing payment info
-- FIXME: Feedback on existing pledges
runMech db (ActStorePledge pptr) = do
    Entity pid p <- db (upsertPatron pptr [])
    maybe noCustomer (checkpledge pid) (pure p <* Model.patronStripeCustomer p)
  where
    checkpledge pid p =
        maybe (pledge' pid) existingPledge (Model.patronPledgeSince p)
    pledge' pid = db $ do
        now <- liftIO getCurrentTime
        update pid [PatronPledgeSince =. Just now]
        insert_ (PledgeHistory pid now CreatePledge)
    noCustomer = pure ()
    existingPledge _ = pure ()

-- FIXME: Feedback on nonexistent pledge.
runMech db (ActDeletePledge pptr) = db $ do
    -- In the absence of triggers or other database use sophistication, we
    -- fetch/evaluate/modify here.
    Entity pid p <- upsertPatron pptr []
    maybe noPledge  (const (delete' pid)) (Model.patronPledgeSince p)
  where
    noPledge = pure ()
    delete' pid = do
        now <- liftIO getCurrentTime
        update pid [PatronPledgeSince =. Nothing]
        insert_ (PledgeHistory pid now DeletePledge)

runMech db ActFetchCrowdCount = db $ Crowd <$> Skeleton.countActivePatrons
runMech db (ActFetchPatron pptr) =
    db $ fromModel . entityVal <$> upsertPatron pptr []

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

stripeCreateCustomer :: MonadIO io => PaymentToken -> StripeT io Customer
stripeCreateCustomer = singleton . CreateCustomerI

stripeUpdateCustomer
    :: MonadIO io => PaymentToken -> CustomerId -> StripeT io Customer
stripeUpdateCustomer tok = singleton . UpdateCustomerI tok

stripeDeleteCustomer :: MonadIO io => CustomerId -> StripeT io ()
stripeDeleteCustomer = singleton . DeleteCustomerI

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
fromModel (Model.Patron usr t c d p) = Patron t c d p

toModel :: ToMechPatron p => p -> Patron -> Model.Patron
toModel usr (Patron t c d p) = Model.Patron (usr ^. from external) t c d p
