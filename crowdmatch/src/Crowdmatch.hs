{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The one-stop module for the Crowdmatch mechanism!
module Crowdmatch (
        -- * Interface with your model
          ToCrowdmatchPatron(..)
        , crowdmatchManualMigrations
        , migrateCrowdmatch
        , SqlRunner

        -- * Interface with stripe
        , StripeRunner
        , runStripe

        -- * Store/delete payment tokens
        , storePaymentToken
        , deletePaymentToken

        -- * Store/delete pledges
        , storePledge
        , deletePledge

        -- * Trigger a crowdmatch
        , crowdmatch

        -- * Data retrieval
        , fetchProject
        , fetchPatron

        -- * Types returned by crowdmatch actions
        , Patron(..)
        , Project(..)
        , DonationUnits(..)
        , HistoryTime(..)
        , Cents(..)
        , PaymentToken(..)

        -- * Internal stuff, mostly for tests
        , CrowdmatchI(..)
        , runMech
        , StripeI(..)
        , PPtr(..)
        , donationCents
        ) where

import Control.Error (ExceptT(..), runExceptT)
import Control.Lens ((^.), from, view, Iso', iso)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Time (UTCTime, getCurrentTime, utctDay)
import Database.Persist
import Database.Persist.Sql (SqlPersistT)
import Web.Stripe (stripe, (-&-), StripeConfig)
import Web.Stripe.Error (StripeError)
import Web.Stripe.Customer
        ( TokenId
        , Customer
        , CustomerId
        , customerId
        , updateCustomer
        , createCustomer
        , deleteCustomer)

import Crowdmatch.Model hiding (Patron(..))
import qualified Crowdmatch.Model as Model
import qualified Crowdmatch.Skeleton as Skeleton

-- | A method that runs 'SqlPersistT' values in your environment.
type SqlRunner io env = forall a. SqlPersistT io a -> env a

-- | A method that runs 'StripeI' instructions in IO. A default that uses
-- 'stripe' is provided by 'runStripe'.
type StripeRunner = forall io.
    MonadIO io => forall a. StripeI a -> io (Either StripeError a)

--
-- THE ACTUAL INTERFACE USED BY THE WEBSITE
--

-- | Information about a particular patron, returned by 'fetchPatron'.
data Patron = Patron
        { patronCreated :: UTCTime
        , patronPaymentToken :: Maybe PaymentToken
        , patronDonationPayable :: DonationUnits
        , patronPledgeSince :: Maybe UTCTime
        }
        deriving (Eq, Show)

-- | Data about a project. There's only one, Snowdrift, so this is rather
-- simple. Returned with 'fetchProject'.
data Project = Project
        { projectCrowd :: Int
        , projectMonthlyIncome :: Cents
        , projectPledgeValue :: DonationUnits
        , projectDonationReceivable :: DonationUnits
        }

-- | Record a 'TokenId' for a patron.
storePaymentToken
    :: (ToCrowdmatchPatron usr, MonadIO io, MonadIO env)
    => SqlRunner io env
    -> StripeRunner
    -> usr -- ^ your model's user, an instance of ToCrowdmatchPatron
    -> TokenId -- ^ you must independently get this from stripe
    -> env (Either StripeError ())
storePaymentToken db strp usr =
    runMech db . StorePaymentTokenI strp (usr ^. from external)

-- NB: The "-- ^" in the following methods is intentional. It forces
-- Haddocks to reformat the arguments in a pleasing way.

-- | Delete the 'TokenId'. This will remove any existing pledges, since a
-- a token is required for pledging.
deletePaymentToken
    :: (ToCrowdmatchPatron usr, MonadIO io, MonadIO env)
    => SqlRunner io env -- ^
    -> StripeRunner
    -> usr
    -> env (Either StripeError ())
deletePaymentToken db strp =
    runMech db . DeletePaymentTokenI strp . (^. from external)

-- | Stores a pledge, joining the crowd. Requires the patron to already
-- have a payment token available.
storePledge
    :: (ToCrowdmatchPatron usr, MonadIO io, MonadIO env)
    => SqlRunner io env -- ^
    -> usr
    -> env ()
storePledge db = runMech db . StorePledgeI . (^. from external)

-- | Delete a pledge, leaving the crowd.
deletePledge
    :: (ToCrowdmatchPatron usr, MonadIO io, MonadIO env)
    => SqlRunner io env -- ^
    -> usr
    -> env ()
deletePledge db = runMech db . DeletePledgeI . (^. from external)

-- | Retrieve info on the project.
fetchProject
    :: (MonadIO io, MonadIO env)
    => SqlRunner io env -- ^
    -> env Project
fetchProject db = runMech db FetchProjectI

-- | Retrieve info on a particular patron.
fetchPatron
    :: (ToCrowdmatchPatron usr, MonadIO io, MonadIO env)
    => SqlRunner io env -- ^
    -> usr
    -> env Patron
fetchPatron db = runMech db . FetchPatronI . (^. from external)

crowdmatch
    :: (MonadIO io, MonadIO env)
    => SqlRunner io env -- ^
    -> env ()
crowdmatch db = runMech db CrowdmatchI

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
    FetchProjectI :: CrowdmatchI Project
    FetchPatronI :: PPtr -> CrowdmatchI Patron
    CrowdmatchI :: CrowdmatchI ()

-- | Executing the actions
runMech
    :: (MonadIO env, MonadIO io)
    => SqlRunner io env -> CrowdmatchI return -> env return

--
-- Payment token (store/delete)
--

runMech db (StorePaymentTokenI strp pptr cardToken) = do
    Entity pid p <- db (upsertPatron pptr [])
    runExceptT $ do
        ret <- ExceptT $ maybe create' update' (Model.patronPaymentToken p)
        ExceptT (Right <$> updatePatron' pid ret)
  where
    create' = stripeCreateCustomer strp cardToken
    update' = stripeUpdateCustomer strp cardToken . unPaymentToken
    updatePatron' pid c = do
        now <- liftIO getCurrentTime
        let payToken = PaymentToken (customerId c)
        db $ do
            _ <- insert (PaymentTokenHistory pid (HistoryTime now) Create)
            update pid [PatronPaymentToken =. Just payToken]

-- FIXME: Feedback on nonexisting CustomerId.
runMech db (DeletePaymentTokenI strp pptr) = do
    Entity pid p <- db (upsertPatron pptr [])
    maybe (pure (Right ())) (deleteToken' pid) (Model.patronPaymentToken p)
  where
    deleteToken' pid (PaymentToken cust) = do
        res <- stripeDeleteCustomer strp cust
        traverse (const (onStripeSuccess' pid)) res
    onStripeSuccess' pid = do
        now <- liftIO getCurrentTime
        -- Must delete pledges if there's no payment method!
        -- Fixme: Duplication of upsert
        runMech db (DeletePledgeI pptr)
        db $ do
            _ <- insert (PaymentTokenHistory pid (HistoryTime now) Delete)
            update pid [PatronPaymentToken =. Nothing]

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
        insert_ (PledgeHistory pid now Create)
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
        insert_ (PledgeHistory pid now Delete)

runMech db FetchProjectI = db $ do
    numPledges <- count [PatronPledgeSince !=. Nothing]
    -- Persistent terrible SQL :|
    receivable <-
        fmap
            (sum . map (Model.patronDonationPayable . entityVal))
            (selectList [] [])
    let pledgevalue = DonationUnits (fromIntegral numPledges)
        income = view donationCents (pledgevalue * pledgevalue)
    pure (Project numPledges income pledgevalue receivable)

runMech db (FetchPatronI pptr) =
    db $ fromModel . entityVal <$> upsertPatron pptr []

runMech db CrowdmatchI = db $ do
    active <- Skeleton.activePatrons
    let projectValue = fromIntegral (length active)
    today <- utctDay <$> liftIO getCurrentTime
    mapM_
        (recordCrowdmatch (CrowdmatchDay today) (DonationUnits projectValue))
        active
  where
    recordCrowdmatch
        :: MonadIO m
        => CrowdmatchDay
        -> DonationUnits
        -> Entity Model.Patron
        -> SqlPersistT m ()
    recordCrowdmatch day amt (Entity pid _) = do
        insert_ (CrowdmatchHistory pid day amt)
        void (update pid [PatronDonationPayable +=. amt])

--
-- I N C E P T I O N
--
-- (one more level down)
-- Wherein we abstract over the possible ways of running Stripe.
--

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

-- | Stripe instructions we use
data StripeI a where
    CreateCustomerI :: TokenId -> StripeI Customer
    UpdateCustomerI :: TokenId -> CustomerId -> StripeI Customer
    DeleteCustomerI :: CustomerId -> StripeI ()

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
upsertPatron
    :: MonadIO m
    => PPtr
    -> [Update Model.Patron]
    -> SqlPersistT m (Entity Model.Patron)
upsertPatron pptr mods = do
    now <- liftIO getCurrentTime
    upsert (Model.Patron pptr now Nothing 0 Nothing) mods

fromModel :: Model.Patron -> Patron
fromModel (Model.Patron _usr t c d p) = Patron t c d p

-- | DonationUnits are truncated to usable cents for use in creating
-- charges.
donationCents :: Iso' DonationUnits Cents
donationCents = iso toCents fromCents
  where
    fromCents = fromIntegral . (* 10)
    toCents = fromIntegral . (`div` 10)
