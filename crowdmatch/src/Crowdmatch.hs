{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Crowdmatch (module Crowdmatch) where

import Control.Error
import Control.Lens
import Control.Monad (void, (<=<), join)
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
type StripeRunner = forall a. StripeI a -> IO (Either StripeError a)

storePaymentToken
    :: (ToMechPatron usr, MonadIO io, MonadIO env)
    => SqlRunner io env
    -> StripeRunner
    -> usr
    -> TokenId
    -> env (Either StripeError ())
storePaymentToken db strp usr =
    runMech db . StorePaymentTokenI strp (usr ^. from external)

deletePaymentToken
    :: (ToMechPatron usr, MonadIO io, MonadIO env)
    => SqlRunner io env
    -> StripeRunner
    -> usr
    -> env (Either StripeError ())
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
        -> TokenId
        -> CrowdmatchI (Either StripeError ())
    DeletePaymentTokenI
        :: StripeRunner
        -> PPtr
        -> CrowdmatchI (Either StripeError ())
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
runMech db (StorePaymentTokenI strp pptr cardToken) = do
    Entity pid p <- db (upsertPatron pptr [])
    runExceptT $ do
        ret <- ExceptT $ maybe create' update' (Model.patronPaymentToken p)
        ExceptT (Right <$> updatePatron' pid ret)
        -- updatePatron' pid ret
  where
    create' = stripeCreateCustomer strp cardToken
    update' = stripeUpdateCustomer strp cardToken . unPaymentToken
    updatePatron' pid c =
        let payToken = PaymentToken (customerId c)
        in db (update pid [PatronPaymentToken =. Just payToken])

-- FIXME: Feedback on nonexisting CustomerId.
runMech db (DeletePaymentTokenI strp pptr) = do
    Entity pid p <- db (upsertPatron pptr [])
    maybe (pure (Right ())) (deleteToken' pid) (Model.patronPaymentToken p)
    -- fmap join (traverse (deleteToken' pid) (Model.patronPaymentToken p))
  where
    deleteToken' pid (PaymentToken cust) = do
        res <- stripeDeleteCustomer strp cust
        traverse (const (update' pid)) res
    update' pid = do
        -- Must delete pledges if there's no payment method!
        -- Fixme: Duplication of upsert
        runMech db (DeletePledgeI pptr)
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
    CreateCustomerI :: TokenId -> StripeI Customer
    UpdateCustomerI :: TokenId -> CustomerId -> StripeI Customer
    DeleteCustomerI :: CustomerId -> StripeI ()

stripeCreateCustomer
    :: MonadIO io => StripeRunner -> TokenId -> io (Either StripeError Customer)
stripeCreateCustomer strp = liftIO . strp . CreateCustomerI

stripeUpdateCustomer
    :: MonadIO io
    => StripeRunner
    -> TokenId
    -> CustomerId
    -> io (Either StripeError Customer)
stripeUpdateCustomer strp tok = liftIO . strp . UpdateCustomerI tok

stripeDeleteCustomer
    :: MonadIO io
    => StripeRunner
    -> CustomerId
    -> io (Either StripeError ())
stripeDeleteCustomer strp = liftIO . strp . DeleteCustomerI


-- | A default stripe runner
runStripe
    :: MonadIO io
    => StripeConfig -> StripeI a -> io (Either StripeError a)
runStripe c = \case
    CreateCustomerI cardToken ->
        liftIO (stripe c (createCustomer -&- cardToken))
    UpdateCustomerI cardToken cust ->
        liftIO (stripe c (updateCustomer cust -&- cardToken))
    DeleteCustomerI cust ->
        void <$> liftIO (stripe c (deleteCustomer cust))

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
