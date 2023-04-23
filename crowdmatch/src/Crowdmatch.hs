{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The one-stop module for the Crowdmatch mechanism!
module Crowdmatch (
        -- * Interface with your model
          ToCrowdmatchPatron(..)
        , crowdmatchManualMigrations
        , migrateCrowdmatch
        , SqlRunner

        -- * Interface with stripe
        , StripeActions (..)
        , stripeProduction
        , StripeDevState (..)

        -- * Store/delete payment tokens
        , storePaymentToken
        , deletePaymentToken

        -- * Store/delete pledges
        , storePledge
        , deletePledge

        -- * Utilities designed to be used from the UNIX environment (errors go
        -- to stderr, etc.)

        , crowdmatch
        , makePayments

        -- * Data retrieval
        , fetchProject
        , fetchPatron
        , fetchPatronPayouts
        , minimumDonation
        , patronMaxDonation

        -- * Types returned by crowdmatch actions
        , Patron(..)
        , PatronId
        , Project(..)
        , DonationUnits(..)
        , HistoryTime(..)
        , CrowdmatchDay(..)
        , Cents(..)
        , PaymentToken(..)

        -- * Internal stuff, mostly for tests
        , CrowdmatchI(..)
        , runMech
        , PPtr(..)
        , centsToUnits
        , unitsToCents
        , sufficientDonation
        ) where

import Control.Error (ExceptT(..), runExceptT, note)
import Control.Lens ((^.), from)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Function (on)
import Data.Int (Int32)
import Data.Ratio
import Data.Time (UTCTime(..), getCurrentTime)
import Data.Time.Calendar (Day)
import Database.Persist
import Database.Persist.Sql (SqlPersistT)
import System.IO
import Web.Stripe (Expandable (..), StripeError)
import Web.Stripe.Balance (BalanceTransaction (..))
import Web.Stripe.Charge (Charge (..))
import Web.Stripe.Customer (TokenId, CustomerId, Customer (..))

import Crowdmatch.Model hiding (Patron(..))
import Crowdmatch.Stripe
import qualified Crowdmatch.Model as Model
import qualified Crowdmatch.Skeleton as Skeleton

-- This is run by the doctests and sets everything up in there:
--
-- $setup
-- >>> import Test.QuickCheck
-- >>> instance Arbitrary Cents where arbitrary = Cents . getPositive <$> arbitrary

-- | A method that runs 'SqlPersistT' values in your environment.
type SqlRunner io env = forall a. SqlPersistT io a -> env a

--
-- THE ACTUAL INTERFACE USED BY THE WEBSITE
--

-- | Information about a particular patron, returned by 'fetchPatron'.
data Patron = Patron
        { patronCreated :: UTCTime
        , patronPaymentToken :: Maybe PaymentToken
        , patronDonationPayable :: DonationUnits
        , patronPledgeSince :: Maybe UTCTime
        , patronCrowdmatches :: [(CrowdmatchDay, DonationUnits)]
        }
        deriving (Eq, Show)

-- TODO: convert into patronDonations property on `Patron` type above
type PayoutHistory = [(HistoryTime, DonationUnits, Cents)]

-- | Data about a project. There's only one, Snowdrift, so this is rather
-- simple. Returned with 'fetchProject'.
data Project = Project
        { projectCrowd :: Int
        , projectMonthlyIncome :: DonationUnits
        , projectPledgeValue :: DonationUnits
        , projectDonationReceivable :: DonationUnits
        , projectDonationsReceived :: DonationUnits
        }

-- | Record a 'TokenId' for a patron.
storePaymentToken
    :: (ToCrowdmatchPatron usr, MonadIO env)
    => StripeActions
    -> usr -- ^ your model's user, an instance of ToCrowdmatchPatron
    -> TokenId -- ^ you must independently get this from stripe
    -> SqlPersistT env (Either StripeError ())
storePaymentToken strp usr =
    runMech . StorePaymentTokenI strp (usr ^. from external)

-- NB: The "-- ^" in the following methods is intentional. It forces
-- Haddocks to reformat the arguments in a pleasing way.

-- | Delete the 'TokenId'. This will remove any existing pledges, since a
-- a token is required for pledging.
deletePaymentToken
    :: (ToCrowdmatchPatron usr, MonadIO env)
    => StripeActions -- ^
    -> usr
    -> SqlPersistT env (Either StripeError ())
deletePaymentToken strp =
    runMech . DeletePaymentTokenI strp . (^. from external)

-- | Stores a pledge, joining the crowd. Requires the patron to already
-- have a payment token available.
storePledge
    :: (ToCrowdmatchPatron usr, MonadIO env)
    => usr -- ^
    -> SqlPersistT env ()
storePledge = runMech . StorePledgeI . (^. from external)

-- | Delete a pledge, leaving the crowd.
deletePledge
    :: (ToCrowdmatchPatron usr, MonadIO env)
    => usr -- ^
    -> SqlPersistT env ()
deletePledge = runMech . DeletePledgeI . (^. from external)

-- | Retrieve info on the project.
fetchProject
    :: MonadIO env
    => SqlPersistT env Project
fetchProject = runMech FetchProjectI

-- | Retrieve info on a particular patron.
fetchPatron
    :: (ToCrowdmatchPatron usr, MonadIO env)
    => usr -- ^
    -> SqlPersistT env Patron
fetchPatron = runMech . FetchPatronI . (^. from external)

-- | Retrieve Rest of patron info
--
-- This should really just be merged with fetchPatron.
-- However, it was added at a time when we had very little Haskell expertise,
-- so the path of least resistance to minimize type errors was to add an
-- entirely new code path, to avoid the need to change other code.
fetchPatronPayouts
    :: (ToCrowdmatchPatron usr, MonadIO env)
    => usr -- ^
    -> SqlPersistT env PayoutHistory
fetchPatronPayouts = runMech . FetchPatronPayoutHistoryI . (^. from external)

-- | Execute a crowdmatch event
crowdmatch
    :: MonadIO env
    => Day
    -> SqlPersistT env ()
crowdmatch day = runMech $ CrowdmatchI day

-- | Execute a payments event, sending charge commands to Stripe.
-- This is just an external wrapper around runMech
--
-- This holds a lock on the database to ensure consistency. That could kill
-- concurrent performance, but right now the only thing hitting the payment
-- tables is this utility and the crowdmatch utility. None of those should ever
-- be run simultaneously at present, so I'd rather have bad "performance" on
-- operational mistakes, rather than bad/duplicate charges. :)
makePayments
    :: MonadIO env
    => StripeActions -- ^
    -> Bool
    -> SqlPersistT env ()
makePayments strp teamOnly = runMech (MakePaymentsI strp teamOnly)

--
-- ONE LEVEL DOWN
-- wherein we use our internal, Markov-able api
--

-- | Actions provided by the library
data CrowdmatchI return where
    StorePaymentTokenI
        :: StripeActions
        -> PPtr
        -> TokenId
        -> CrowdmatchI (Either StripeError ())
    DeletePaymentTokenI
        :: StripeActions
        -> PPtr
        -> CrowdmatchI (Either StripeError ())
    StorePledgeI :: PPtr -> CrowdmatchI ()
    DeletePledgeI :: PPtr -> CrowdmatchI ()
    FetchProjectI :: CrowdmatchI Project
    FetchPatronI :: PPtr -> CrowdmatchI Patron
    FetchPatronPayoutHistoryI :: PPtr -> CrowdmatchI PayoutHistory
    CrowdmatchI :: Day -> CrowdmatchI ()
    MakePaymentsI :: StripeActions -> Bool -> CrowdmatchI ()

-- | Executing the actions
runMech :: MonadIO env => CrowdmatchI return -> SqlPersistT env return

--
-- Payment token (store/delete)
--

runMech (StorePaymentTokenI strp pptr cardToken) = do
    Entity pid p <- upsertPatron pptr []
    runExceptT $ do
        ret <- ExceptT $ maybe create' update' (Model.patronPaymentToken p)
        ExceptT (Right <$> updatePatron' pid ret)
  where
    create' = liftIO $ createCustomer strp cardToken
    update' = liftIO . updateCustomer strp cardToken . unPaymentToken
    updatePatron' pid c = do
        now <- liftIO getCurrentTime
        let payToken = PaymentToken (customerId c)
        _ <- insert (PaymentTokenHistory pid (HistoryTime now) Create)
        update pid [PatronPaymentToken =. Just payToken]

-- FIXME: Feedback on nonexisting CustomerId.
runMech (DeletePaymentTokenI strp pptr) = do
    Entity pid p <- upsertPatron pptr []
    maybe (pure (Right ())) (deleteToken' pid) (Model.patronPaymentToken p)
  where
    deleteToken' pid (PaymentToken cust) = do
        res <- liftIO $ deleteCustomer strp cust
        traverse (const (onStripeSuccess' pid)) res
    onStripeSuccess' pid = do
        now <- liftIO getCurrentTime
        -- Must delete pledges if there's no payment method!
        -- Fixme: Duplication of upsert
        runMech (DeletePledgeI pptr)
        _ <- insert (PaymentTokenHistory pid (HistoryTime now) Delete)
        update pid [PatronPaymentToken =. Nothing]

--
-- Pledge (store/delete)
--

-- FIXME: Feedback on missing payment info
-- FIXME: Feedback on existing pledges
runMech (StorePledgeI pptr) = do
    Entity pid p <- upsertPatron pptr []
    maybe noCustomer (checkpledge pid) (pure p <* Model.patronPaymentToken p)
  where
    checkpledge pid p =
        maybe (pledge' pid) existingPledge (Model.patronPledgeSince p)
    pledge' pid = do
        now <- liftIO getCurrentTime
        update pid [PatronPledgeSince =. Just now]
        insert_ (PledgeHistory pid now Create)
    noCustomer = pure ()
    existingPledge _ = pure ()

-- FIXME: Feedback on nonexistent pledge.
runMech (DeletePledgeI pptr) = do
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

runMech FetchProjectI = do
    numPledges <- count [PatronPledgeSince !=. Nothing]
    receivable <- Skeleton.sumField PatronDonationPayable
    -- This should be verified against Stripe. This calculation is nothing more
    -- than a "guess".
    received <- Skeleton.sumField DonationHistoryAmount

    let pledgevalue = DonationUnits (fromIntegral numPledges)
        income = pledgevalue * (fromIntegral numPledges)
    pure (Project numPledges income pledgevalue receivable received)

runMech (FetchPatronI pptr) = do
    Entity pid p <- upsertPatron pptr []
    hist <- fmap (map entityVal)
        (selectList
            [Model.CrowdmatchHistoryPatron ==. pid]
            [Asc Model.CrowdmatchHistoryDate])
    return (fromModel p hist)

runMech (FetchPatronPayoutHistoryI pptr) = do
    Entity pid _ <- upsertPatron pptr []
    hist <- fmap (map entityVal)
        (selectList
            [Model.DonationHistoryPatron ==. pid]
            [Asc Model.DonationHistoryTime])
    return (map values hist)
  where
    values DonationHistory{..} =
        (donationHistoryTime, donationHistoryAmount, donationHistoryFee)

--
-- Crowdmatch and MakePayments
--

runMech (CrowdmatchI date) = do
    -- Active patrons are those who have pledged before midnight (the 0 in the
    -- line below) of the crowdmatch day
    active <- Skeleton.activePatrons (UTCTime date 0)
    let projectValue = fromIntegral (length active)
    mapM_
        (recordCrowdmatch (CrowdmatchDay date) (DonationUnits projectValue))
        active
  where
    recordCrowdmatch day amt (Entity pid _) = do
        insert_ (CrowdmatchHistory pid day amt)
        void (update pid [PatronDonationPayable +=. amt])

runMech (MakePaymentsI strp teamOnly) = do
    -- Duplicating sql logic with Haskell logic to get rid of patrons
    -- without a Stripe CustomerId :/
    -- get patrons who have high enough outstanding balance
    chargeable <- (if teamOnly then Skeleton.teamMembersReceivable teamMemberMinimumDonation else Skeleton.patronsReceivable minimumDonation)
    let donors =
            map -- over chargeable patrons
                (\(Entity pId p) -> note pId -- If the Donor below is Nothing, use just the id
                    (Donor -- Maybe "ChargeablePatron" would be a better name
                     <$> Just pId -- first element of Donor record: our system
                     -- unwrap paymentToken to basic type (string?)
                     <*> fmap unPaymentToken (Model.patronPaymentToken p) -- Stripe customerID
                     <*> Just (Model.patronDonationPayable p))) -- Amount they owe, extracted from chargeable
                chargeable
        -- Run the charges via stripe api
        -- traverse :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)
        -- Data.Traversable:
        --   traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
        -- traverse: like fmap, but allows you to run effects while collecing results
        --           which we need because sendCharge runs actions (hit stripe api)
        -- `Either` is traversable, hence nested traverse. Its signature:
        -- traverse :: Applicative f => (a0 -> f b) -> Either a a0 -> f (Either a b)
        -- maps the second Either value (OK), keeps the first (the error) as-is
        -- Traversing over the either is the action which we then apply over the list of donors
        -- One is ignoring errors, the other is mapping over the list
        -- End result is a list of eithers
        -- ListLike<Either StripeError ChargeResult>
        chargeAllDonorsAction = traverse (traverse sendCharge) donors
        -- There are more outcomes than just NoPaymentInfo, ChargeFailure, and PayOk
        -- But we'll just consider those 3 => summarized/flattened as PaymentOutcome
        --
        -- https://hackage.haskell.org/package/stripe-core-2.2.2/docs/Web-Stripe-Error.html#t:StripeError
        --
        -- If chargeAction is StripeError -> NoPaymentInfo(StripeError) -- should really be BadPaymentInfo
        -- If ChargeAction is StripeReturn ->
        --      If StripeReturn is Charge -> PayOk(Charge)
        --      Else -> ChargeFailure(StripeReturn)
        --
        -- sendCharge: -> io (Either StripeError ChargeResult)
        -- ChargeResult: (Either StripeError (StripeReturn a))
        simplifiedOutcomes = -- List of PaymentOutcome
            fmap
                -- either :: fn1(a -> c) -> fn2(b -> c) -> Either a b -> c
                -- here, c is PaymentOutcome
                (map (either NoPaymentInfo (either ChargeFailure PayOk)))
                chargeAllDonorsAction
    chargeResults <- liftIO simplifiedOutcomes
    -- Monadic version of traverse, essentially does the same thing
    -- mapM :: (Traversable t, Monad m) => (a -> m b) -> t a -> m (t b)
    -- mapM_ :: (Foldable t, Monad m) => (a -> m b) -> t a -> m ()
    -- Ends up returning success if everyone succeeds or error if any failures (we think)
    mapM_ recordResults chargeResults
  where
    -- | Send the charge command to Stripe
    --
    -- For the Futurama milestone, we tack on a fee that covers the Stripe fee
    -- to calculate the 'payment'. (because normally stripe just takes the fee
    -- out of the project income, but we want the project to get the amount that
    -- crowdmatching said it would get)
    sendCharge
        :: MonadIO io
        => Donor
        -> io (Either StripeError ChargeResult)
    sendCharge Donor{..} = -- Donor{_donorPatron, _donorCustomer, _donorDonationPayable}
        fmap
            -- ChargeResult = patron, fee, net, charge
            -- If we get a stripeError from chargeCustomer, return that stripeerror
            -- If not, return the charge, wrapped in a ChargeResult
            (fmap (ChargeResult _donorPatron fee net))
          -- liftIO :: MonadIO m => IO a -> m a
          -- liftIO is just here to make monad types match, nothing functional
            (liftIO $ chargeCustomer strp _donorCustomer cents)
      where
        cents = payment (unitsToCents _donorDonationPayable)
        fee = stripeFee cents
        net = centsToUnits (cents - fee)

    recordResults
        :: (MonadIO m, Show b, Show c)
        => PaymentOutcome ChargeResult b c
        -> SqlPersistT m ()
    -- TODO: Record in DB instead of just logging to stderr
    recordResults = \case
        -- Just log their id
        NoPaymentInfo pId ->
            liftIO
                (hPrint
                    stderr
                    ("Skipped patron with no payment info: " ++ show pId))
        -- Log everything we get from stripe
        ChargeFailure e -> liftIO (hPrint stderr e)

        PayOk chargeResult -> recordDonation chargeResult
      where
        recordDonation ChargeResult{..} = do
            ts <- stripeDonationTimestamp strp _chargeResultCharge
            insert_
                (DonationHistory
                    _chargeResultPatron
                    ts
                    _chargeResultNet
                    _chargeResultFee)
            update
                _chargeResultPatron
                [PatronDonationPayable -=. _chargeResultNet]

--
-- I N C E P T I O N
--
-- (one more level down)
-- Wherein we abstract over the possible ways of running Stripe.
--

-- | Tries to get the timestamp from the Charge's TransactionBalance
-- sub-item. If that fails, it's cool, we'll just use a local variant of
-- "now".
--
-- I don't want to bail on recording the charge if we can't get the
-- timestamp, since the presence of the Charge itself means Stripe
-- processed it. There was merely a secondary failure getting the
-- TransactionBalance. Ideally we'd retry, with some sort of 'pending'
-- status, but let's slap that together later.
stripeDonationTimestamp
    :: MonadIO io
    => StripeActions -> Charge -> io HistoryTime
stripeDonationTimestamp strp charge = fmap HistoryTime (chargeTime charge)
  where
    fallback = liftIO getCurrentTime
    chargeTime Charge{..} =
        maybe fallback transactionTime chargeBalanceTransaction
    transactionTime = \case
        Expanded BalanceTransaction{..} -> pure balanceTransactionCreated
        Id transId -> (=<<)
            (either (const fallback) (pure . balanceTransactionCreated))
            (liftIO $ balanceTransaction strp transId)

--
-- Making payments
--

data ChargeResult = ChargeResult
        { _chargeResultPatron :: PatronId
        , _chargeResultFee :: Cents
        , _chargeResultNet :: DonationUnits
        , _chargeResultCharge :: Charge
        } deriving (Show)

-- | Calculate Stripe's fee: 2.9% + 30¢
--
-- https://stripe.com/us/pricing
--
-- Stripe uses financial rounding, aka the rounding everyone outside the US
-- learns (apparently). This is the rounding implemented in Prelude, as
-- well. Hooray!
--
-- If we ever have integration testing, we should confirm the following
-- holds true:
--
--      $5.00 charge -> 44.5¢ fee -> Stripe rounded to 44¢
--     $15.00 charge -> 73.5¢ fee -> Stripe rounded to 74¢
--
-- I confirmed these facts when I wrote this function, but tests ftw.
stripeFee :: Cents -> Cents
stripeFee = Cents . (+ 30) . round' . (% 1000) . (* 29) . fromIntegral
  where
    -- Monomorphize the type
    round' :: Rational -> Int32
    round' = round

-- | As of 2016-10-10, the amount a patron pays is increased so that the
-- amount the project receives is equal to the amount they crowdmatched.
--
-- Proving that the rounding always works out was annoying, but I did it
-- with a brute-force program. It's ok up until integer overflows around
-- ~$20M.
--
-- prop> \d -> d < 2*10^9 ==> let {p = payment d; f = stripeFee p} in p-f==d
payment :: Cents -> Cents
payment = Cents . round' . (% (1000-29)) . (* 1000) . (+ 30) . fromIntegral
  where
    -- monomorphize
    round' :: Rational -> Int32
    round' = round

-- | A donation is sufficient for processing if the Stripe fee is < 10%.
-- https://tree.taiga.io/project/snowdrift/issue/457
--
-- This function is useful for testing, but we memoize its
-- production-required result below.
--
-- Since we're using the 'payment' function right now, this equation is
-- different from the long term ideal.
sufficientDonation :: DonationUnits -> Bool
sufficientDonation d =
    fee % p < maximumFee
  where
    p = payment (unitsToCents d)
    fee = stripeFee p
    maximumFee = ((%) `on` Cents) 1 10

-- | This is the minimum amount that satisfies 'sufficientDonation'. You can
-- find it for yourself by running:
-- >>> :{
-- >>> let x = head . filter (\x -> all sufficientDonation [x..x+35])
-- >>>              . map DonationUnits
-- >>>              $ [10..]
-- >>> in (x, x == minimumDonation)
-- >>>:}
-- (DonationUnits 3790,True)
--
-- Note that rounding makes the function discontinuous, with a step every
-- 1/0.029 ~ 35 DonationUnits. There's a local optimum at ~3610, but we'll just
-- skip that one, cause that's weird.
--
-- Since we're using the 'payment' function right now, this value is higher
-- than the long term ideal.
minimumDonation :: DonationUnits
minimumDonation = DonationUnits 3790

-- Adroit was below the threshold and we wanted to charge him anyway
teamMemberMinimumDonation :: DonationUnits
teamMemberMinimumDonation = 3000

-- | This is currently hardcoded.
patronMaxDonation :: DonationUnits
patronMaxDonation = DonationUnits 10000

-- | The projection of a Patron that can, and should, make a donation.
data Donor = Donor
        { _donorPatron :: PatronId
        , _donorCustomer :: CustomerId
        , _donorDonationPayable :: DonationUnits
        } deriving (Show)

data PaymentOutcome a b c = PayOk a | NoPaymentInfo b | ChargeFailure c

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

-- | Build an external representation of a patron from our internal parts.
fromModel :: Model.Patron -> [Model.CrowdmatchHistory] -> Patron
fromModel (Model.Patron _usr t c d p) = (Patron t c d p) . map values
  where
    values CrowdmatchHistory{..} =
        (crowdmatchHistoryDate, crowdmatchHistoryAmount)

-- | DonationUnits are truncated to usable cents for use in creating
-- charges.
unitsToCents :: DonationUnits -> Cents
unitsToCents = fromIntegral . (`div` 10)

centsToUnits :: Cents -> DonationUnits
centsToUnits = fromIntegral . (* 10)
