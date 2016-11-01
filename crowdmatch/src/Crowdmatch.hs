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
        , patronPaymentToken :: Maybe PaymentToken
        , patronDonationPayable :: DonationUnits
        , patronPledgeSince :: Maybe UTCTime
        }
        deriving (Eq, Show)

data Crowd = Crowd { crowdSize :: Int }

type SqlRunner io env = forall a. SqlPersistT io a -> env a
type StripeRunner = forall a. StripeT IO a -> IO (Either StripeError a)

storePaymentToken
    :: (ToMechPatron usr, MonadIO io, MonadIO env)
    => SqlRunner io env
    -> StripeRunner
    -> usr
    -> PaymentToken
    -> env ()
storePaymentToken db strp usr =
    runMech db . StorePaymentTokenI strp (usr ^. from external)

deletePaymentToken
    :: (ToMechPatron usr, MonadIO io, MonadIO env)
    => SqlRunner io env
    -> StripeRunner
    -> usr
    -> env ()
deletePaymentToken db strp =
    runMech db . DeletePaymentTokenI strp . (^. from external)

storePledge
    :: (ToMechPatron usr, MonadIO io, MonadIO env)
    => SqlRunner io env
    -> usr
    -> env ()
storePledge db = runMech db . StorePledgeI . (^. from external)

deletePledge
    :: (ToMechPatron usr, MonadIO io, MonadIO env)
    => SqlRunner io env
    -> usr
    -> env ()
deletePledge db = runMech db . DeletePledgeI . (^. from external)

fetchCrowdCount
    :: (MonadIO io, MonadIO env)
    => SqlRunner io env
    -> env Crowd
fetchCrowdCount db = runMech db FetchCrowdCountI

fetchPatron
    :: (ToMechPatron usr, MonadIO io, MonadIO env)
    => SqlRunner io env
    -> usr
    -> env Patron
fetchPatron db = runMech db . FetchPatronI . (^. from external)

--
-- ONE LEVEL DOWN
-- wherein we use our internal, Markov-able api
--

-- | Actions provided by the library
data CrowdmatchI return where
    StorePaymentTokenI
        :: StripeRunner
        -> PPtr
        -> PaymentToken
        -> CrowdmatchI ()
    DeletePaymentTokenI :: StripeRunner -> PPtr -> CrowdmatchI ()
    StorePledgeI :: PPtr -> CrowdmatchI ()
    DeletePledgeI :: PPtr -> CrowdmatchI ()
    FetchCrowdCountI :: CrowdmatchI Crowd
    FetchPatronI :: PPtr -> CrowdmatchI Patron

-- | Executing the actions
runMech
    :: (MonadIO env, MonadIO io)
    => SqlRunner io env -> CrowdmatchI return -> env return

--
-- PaymentToken (store/delete)
--

-- FIXME: Feedback on Stripe error
runMech db (StorePaymentTokenI strp pptr tok) = do
    Entity pid p <- db (upsertPatron pptr [])
    ret <- maybe create' update' (Model.patronPaymentToken p)
    either (const (pure ())) (updatePatron' pid) ret
  where
    create' = liftIO (strp (stripeCreateCustomer tok))
    update' = liftIO . strp . stripeUpdateCustomer tok . unPaymentToken
    updatePatron' pid c =
        db (update pid [PatronPaymentToken =. Just tok])

-- FIXME: Feedback on Stripe error or nonexisting CustomerId.
runMech db (DeletePaymentTokenI strp pptr) = do
    Entity pid p <- db (upsertPatron pptr [])
    -- Fixme: Duplication of actions
    -- Must delete pledges if there's no payment method!
    runMech db (DeletePledgeI pptr)
    maybe (pure ()) (deleteCust' pid) (Model.patronPaymentToken p)
  where
    deleteCust' pid (PaymentToken cust) = do
        liftIO (strp (stripeDeleteCustomer cust))
        now <- liftIO getCurrentTime
        db (update pid [PatronPaymentToken =. Nothing])

--
-- Pledge (store/delete)
--

-- FIXME: Feedback on missing payment info
-- FIXME: Feedback on existing pledges
runMech db (StorePledgeI pptr) = do
    Entity pid p <- db (upsertPatron pptr [])
    maybe noCustomer (checkpledge pid) (pure p <* Model.patronPaymentToken p)
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
runMech db (DeletePledgeI pptr) = db $ do
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

runMech db FetchCrowdCountI =
    db (Crowd <$> count [PatronPledgeSince !=. Nothing])
runMech db (FetchPatronI pptr) =
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
