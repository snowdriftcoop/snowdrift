{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-orphans -Wno-missing-fields #-}

import Control.Concurrent (newEmptyMVar, newMVar, MVar, modifyMVar_)
import Control.Exception.Safe (bracket, catch)
import Control.Monad (void, (<=<))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Data.Foldable (traverse_, for_)
import Data.List (partition)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Time (UTCTime(..), getCurrentTime, utctDay)
import Database.Persist
import Database.Persist.Postgresql
        (SqlPersistT, runMigration, unSingle, rawSql, rawExecute)
import RunPersist (runPersistPool)
import System.Directory (createDirectoryIfMissing)
import System.Environment (setEnv)
import Test.Hspec
        ( runIO
        , hspec
        , before_
        , Spec
        , describe
        , it
        , shouldBe
        , shouldNotBe
        , specify)
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck
        ( Arbitrary(..)
        , listOf
        , elements
        , Property
        , choose
        , label
        , Blind(..)
        , getBlind
        , frequency
        , getPositive
        , counterexample)
import Test.QuickCheck.Monadic (PropertyM, monadicIO, run, pick, monitor, assert)
import Web.Stripe.Customer
import Web.Stripe.Balance
import Web.Stripe.Charge
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.Types as PG

import Crowdmatch
import Crowdmatch.Model (StorageAction(..))
import qualified Crowdmatch.Model as Model

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}


-- | Use this instead of actually talking to Stripe during tests. Uses an MVar
-- to maintain stripe's internal state.
stripeDevelopment :: MVar StripeDevState -> StripeActions
stripeDevelopment state = StripeActions
    { createCustomer =
        \ _tok -> pure (Right Customer { customerId = CustomerId "dummy" })
    , updateCustomer =
        \ _tok cust -> pure (Right Customer { customerId = cust })
    , deleteCustomer =
        \ _ -> pure (Right ())
    , chargeCustomer =
        \ _ c -> do
            modifyMVar_ state
                (\StripeDevState {..} ->
                    pure StripeDevState { lastCharge = Just c, .. })
            pure (Right Charge{ chargeBalanceTransaction = Nothing })
    , balanceTransaction =
        \ _ -> pure (Right BalanceTransaction{})
    }

-- | Execute a transaction-free statement in Postgres
pgExecute_ q = bracket open' PG.close (void . (`PG.execute_` q))
  where
    open' = PG.connectPostgreSQL ""

instance Arbitrary Text where
    arbitrary = T.pack <$> listOf (elements ['a'..'z'])

instance Arbitrary PaymentToken where
    arbitrary = fmap (PaymentToken . CustomerId) arbitrary

instance Arbitrary TokenId where
    arbitrary = TokenId <$> arbitrary

type Runner = SqlRunner IO IO

-- | A QuickCheck property that uses database actions
dbProp
    :: SqlPersistT IO () -> Runner -> (Runner -> PropertyM IO ()) -> Property
dbProp truncation runner prop' =
    monadicIO (run (runner truncation) >> prop' runner)

-- | Generate Arbitrary crowdmatch history
genHistory :: Runner -> PropertyM IO ()
genHistory runner = do
    targets <- pick (map HarnessUser <$> listOf (choose (1,1000)))
    let bucket = length targets `div` 5
        b0 = show (bucket * 5)
        b1 = show ((bucket + 1) * 5)
    monitor (label (concat ["N ∈ [", b0,", ",b1 , "]"]))
    dummyStripe <-
        liftIO $ stripeDevelopment <$> newEmptyMVar
    let storeToken' x =
            void . runner . storePaymentToken dummyStripe x <$> arbitrary
        delToken' = pure . void . runner . deletePaymentToken dummyStripe
        storePledge' = pure . runner . storePledge
        delPledge' = pure . runner . deletePledge
        oneAct x =
            -- When new API methods are added, they need to be added here. This
            -- is a problem I don't have a solution to right now.
            (run . getBlind <=< pick) (Blind <$> frequency
                [ (10, storeToken' x)
                , (1, delToken' x)
                , (2, storePledge' x)
                , (1, delPledge' x)
                ])
    -- Running this concurrently causes "impossible" conflicts during
    -- upsert! This needs to be fixed! But on the threat matrix, it is both
    -- unlikely to happen and has minimal side effects (since the DB stays
    -- consistent, after all.) So I'm punting for now.
    traverse_ oneAct targets

-- | Something to make a Patron out of
newtype HarnessUser = HarnessUser Int
    deriving (Eq, Show)

instance ToCrowdmatchPatron HarnessUser where
    toMechPatron (HarnessUser i) = i
    fromMechPatron = HarnessUser

instance Arbitrary HarnessUser where
    arbitrary = HarnessUser . getPositive <$> arbitrary

-- | Run the tests
main :: IO ()
main = withTestDatabase $ runPersistPool $ \runner -> do
    trunq <- runner $ do
        runMigration migrateCrowdmatch
        buildTruncQuery
    -- The manual migrations do not compose with Persistent. Have to go
    -- down to postgresql-simple.
    --
    -- Note how hacky this is! The truncation query above is generated
    -- before these manual migrations, which add a new table. That new
    -- table is thus not truncated. But that's good, because the table in
    -- question is keeping track of manual migrations! This compounds the
    -- problems with the current design of manual migrations.
    bracket (PG.connectPostgreSQL "") PG.close crowdmatchManualMigrations
    hspec $ before_ (runner trunq) $ do
        depTests runner
        unitTests runner
        propTests runner trunq
  where
    -- A query that empties all tables, used before each database test.
    buildTruncQuery = do
        -- esc <- connEscapeName <$> ask
        tables <- fmap
            (map unSingle)
            (rawSql
                ("select table_name"
                 <> " from information_schema.tables"
                 <> " where table_schema = 'public'")
                [])
        let trunq = "truncate table " `mappend` T.intercalate ", " tables
        -- Don't do it yet
        pure (rawExecute trunq [])

-- A harness function
modelPatronWithBalance :: Int -> DonationUnits -> UTCTime -> Model.Patron
modelPatronWithBalance i u t =
        Model.Patron
            (Model.PPtr i)
            t
            (Just (PaymentToken (CustomerId "foo")))
            u
            Nothing

-- | Test upstream deps
depTests :: Runner -> Spec
depTests runner = describe "dep tests" $
    -- | We got bit by this momentarily. Now keeping this test for all of
    -- eternity.
    it "Persistent.upsert != repsert " $ do
        now <- getCurrentTime
        let p1 =
                modelPatronWithBalance 0 (DonationUnits 20) now
            conflict =
                modelPatronWithBalance 0 (DonationUnits 25) now
        conflictedResult <- runner $ do
            void (insert p1)
            upsert conflict []
        shouldBe
            (Model.patronDonationPayable (entityVal conflictedResult))
            (DonationUnits 20)

unitTests :: Runner -> Spec
unitTests runner = describe "unit tests" $ do
    let payTok = PaymentToken (CustomerId "dummy")
        cardTok = TokenId "pumpkin"
        aelfred = HarnessUser 1
    dummyStripe <-
        runIO $ stripeDevelopment <$> newMVar (StripeDevState Nothing)
    describe "stored token" $ do
        it "is retrievable" $ do
            pat <- runner $ do
                _ <- storePaymentToken dummyStripe aelfred cardTok
                fetchPatron aelfred
            patronPaymentToken pat `shouldBe` Just payTok
        it "disappears" $ do
            pat <- runner $ do
                _ <- storePaymentToken dummyStripe aelfred cardTok
                _ <- deletePaymentToken dummyStripe aelfred
                fetchPatron aelfred
            patronPaymentToken pat `shouldBe` Nothing
        it "has history recorded" $ do
            ls <- runner $ do
                _ <- storePaymentToken dummyStripe aelfred cardTok
                _ <- deletePaymentToken dummyStripe aelfred
                map entityVal <$> selectList [] []
            length ls `shouldBe` 2
            Model.paymentTokenHistoryAction (head ls) `shouldBe` Create
            Model.paymentTokenHistoryAction (last ls) `shouldBe` Delete
    describe "fetchPatron" $ do
        it "creates patron if it doesn't exist" $ do
            _ <- runner $ (,) <$> fetchPatron aelfred <*> fetchPatron aelfred
            ps :: [Entity Model.Patron] <- runner $ selectList [] []
            length ps `shouldBe` 1
        it "includes crowdmatch history" $ do
            let pid = 90
            now <- liftIO getCurrentTime
            let tomorrow = succ $ utctDay now
            p <- runner $ do
                insert_
                    (Model.Patron
                        (PPtr pid)
                        now
                        (Just (PaymentToken (CustomerId "")))
                        0
                        (Just now))
                crowdmatch tomorrow
                fetchPatron (HarnessUser pid)
            length (patronCrowdmatches p) `shouldBe` 1
            (fst . head . patronCrowdmatches) p `shouldBe` CrowdmatchDay tomorrow
        it "includes only relevant crowdmatch history" $ do
            let thunor = 0
                woden = 1
            now <- liftIO getCurrentTime
            let tomorrow = succ $ utctDay now
            runner $ for_ [thunor, woden] (\p ->
                insert_
                    (Model.Patron
                        (PPtr p)
                        now
                        (Just (PaymentToken (CustomerId "")))
                        0
                        (Just now)))
            (thunor', woden') <- runner $ do
                crowdmatch tomorrow
                deletePledge (HarnessUser woden)
                crowdmatch tomorrow
                storePledge (HarnessUser woden)
                crowdmatch tomorrow
                t <- fetchPatron (HarnessUser thunor)
                w <- fetchPatron (HarnessUser woden)
                return (t,w)
            length (patronCrowdmatches thunor') `shouldBe` 3
            length (patronCrowdmatches woden') `shouldBe` 2
            (sum . map snd . patronCrowdmatches) thunor'
                `shouldBe` DonationUnits 5
            (sum . map snd . patronCrowdmatches) woden'
                `shouldBe` DonationUnits 4
    specify "stored pledge is retrievable" $ do
        pat <- runner $ do
            _ <- storePaymentToken dummyStripe aelfred cardTok
            storePledge aelfred
            fetchPatron aelfred
        patronPledgeSince pat `shouldNotBe` Nothing
    specify "deleted pledge is retrievable" $ do
        pat <- runner $ do
            _ <- storePaymentToken dummyStripe aelfred cardTok
            storePledge aelfred
            deletePledge aelfred
            fetchPatron aelfred
        patronPledgeSince pat `shouldBe` Nothing
    describe "crowd size" $ do
        it "gets bumped by pledging" $ do
            p <- runner $ do
                _ <- storePaymentToken dummyStripe aelfred cardTok
                storePledge aelfred
                fetchProject
            projectCrowd p `shouldBe` 1
        it "is not affected by a bad pledge" $ do
            p <- runner $ do
                -- No token!
                storePledge aelfred
                fetchProject
            projectCrowd p `shouldBe` 0
        it "shrinks with removal of payment token" $ do
            p1 <- runner $ do
                _ <- storePaymentToken dummyStripe aelfred cardTok
                storePledge aelfred
                projectCrowd <$> fetchProject
            p1 `shouldBe` 1
            p2 <- runner $ do
                _ <- deletePaymentToken dummyStripe aelfred
                projectCrowd <$> fetchProject
            p2 `shouldBe` 0
    specify "10 pledges = 1 cent" $ do
        let mkPledge i = do
                _ <- storePaymentToken dummyStripe (HarnessUser i) cardTok
                storePledge (HarnessUser i)
        val <- runner $ do
            mapM_ mkPledge [1..10]
            projectPledgeValue <$> fetchProject
        val `shouldBe` centsToUnits (Cents 1)
    specify "1000 pledges = $1000 monthly income" $ do
        let mkPledge i = do
                _ <- storePaymentToken dummyStripe (HarnessUser i) cardTok
                storePledge (HarnessUser i)
        val <- runner $ do
            mapM_ mkPledge [1..1000]
            projectMonthlyIncome <$> fetchProject
        val `shouldBe` DonationUnits (1000 * 1000)
    describe "crowdmatch event" $ do
        now <- runIO getCurrentTime
        let tomorrow = succ $ utctDay now
            mkPatron i =
                void (storePaymentToken dummyStripe (HarnessUser i) cardTok)
            mkPledge i = do
                mkPatron i
                storePledge (HarnessUser i)
        -- Two patrons, one active -> 0.01 payable from the patron
        it "only counts patrons who have pledged" $ do
            val <- runner $ do
                mkPatron 1
                mkPledge 2
                crowdmatch tomorrow
                projectDonationReceivable <$> fetchProject
            val `shouldBe` DonationUnits 1
        it "only counts patrons who have not unpledged" $ do
            val <- runner $ do
                mkPledge 1
                mkPledge 2
                deletePledge (HarnessUser 2)
                crowdmatch tomorrow
                projectDonationReceivable <$> fetchProject
            val `shouldBe` DonationUnits 1
        it "only counts pledges made before the crowdmatch date" $ do
            val <- runner $ do
                -- The `mkPledge` creates a pledge with the current timestamp,
                -- which cannot be *before* the current day, so it will not be
                -- counted.
                mkPledge 1
                crowdmatch $ utctDay now
                projectDonationReceivable <$> fetchProject
            val `shouldBe` DonationUnits 0
        it "looks quadratic; 3 patrons = 9 DUs" $ do -- DU = Donation Unit
            val <- runner $ do
                mapM_ mkPledge [1..3]
                crowdmatch tomorrow
                projectDonationReceivable <$> fetchProject
            val `shouldBe` DonationUnits 9
        it "sums over multiple events" $ do
            val <- runner $ do
                mkPatron 1
                mkPledge 2
                crowdmatch tomorrow
                storePledge (HarnessUser 1)
                -- Now 1 is also active
                crowdmatch tomorrow
                projectDonationReceivable <$> fetchProject
            val `shouldBe` DonationUnits 5
        it "creates history" $ do
            u :: [Entity Model.CrowdmatchHistory] <- runner $ do
                mkPledge 1
                crowdmatch tomorrow
                selectList [] []
            length u `shouldBe` 1
    describe "make-payments event" $ do
        now <- runIO getCurrentTime
        it "creates history" $ do
            hs :: [Entity Model.DonationHistory] <- runner $ do
                insert_ (modelPatronWithBalance 1 (DonationUnits 50000) now)
                makePayments dummyStripe False
                selectList [] []
            length hs `shouldBe` 1
        it "zeroes donations" $ do
            p <- runner $ do
                insert_ (modelPatronWithBalance 1 (DonationUnits 5000) now)
                makePayments dummyStripe False
                fetchPatron (HarnessUser 1)
            patronDonationPayable p `shouldBe` DonationUnits 0
        it "accounts for fees" $ do
            h :: Model.DonationHistory <- runner $ do
                insert_ (modelPatronWithBalance 1 (DonationUnits 5000) now)
                makePayments dummyStripe False
                (entityVal . head) <$> selectList [] []
            -- 500 * 2.99% + 30.9 (roughly the effective fee rate, after
            -- adding enough to pay the project the true donation amount)
            Model.donationHistoryFee h `shouldBe` 46
        it "adds to the project what it takes from the patron" $ do
            p <- runner $ do
                insert_ (modelPatronWithBalance 1 (DonationUnits 5000) now)
                makePayments dummyStripe False
                fetchProject
            projectDonationsReceived p `shouldBe` 5000

propTests :: Runner -> SqlPersistT IO () -> Spec
propTests runner trunq = modifyMaxSuccess (* 2) $
    prop "properties given random history"
        $ dbProp trunq runner prop_randomHistory

-- | We set up an in-memory database to run tests Fast Enough™.
withTestDatabase :: IO () -> IO ()
withTestDatabase = bracket setup' teardown' . const
  where
    db = "crowdmatch_test"
    ts = "crowdmatch_tests"
    shmLoc = "/dev/shm/crowdmatch_test_tablespace"
    setup' = (do
        createDirectoryIfMissing True shmLoc
        setEnv "PGDATABASE" "template1"
        -- Postgres requires separate queries here
        pgExecute_
            ("create tablespace "
                <> ts
                <> " location '"
                <> PG.Query (B.pack shmLoc)
                <> "'")
        pgExecute_
            ("create database "
                <> db
                <> " tablespace "
                <> ts)
        -- Used for the tests
        setEnv "PGDATABASE" (B.unpack (PG.fromQuery db)))
        `catch` (\ (ex :: PG.SqlError) -> do
            putStrLn "Setting up in-memory database failed:"
            print ex
            putStrLn "Falling back to PGDATABASE=snowdrift_test."
            setEnv "PGDATABASE" "snowdrift_test")
    teardown' = const $ (do
        setEnv "PGDATABASE" "template1"
        pgExecute_ ("drop database " <> db)
        pgExecute_ ("drop tablespace " <> ts))
        `catch` (\ (ex :: PG.SqlError) -> do
            putStrLn "Tearing down in-memory database failed:"
            print ex
            putStrLn "Ignoring failure.")

prop_randomHistory :: Runner -> PropertyM IO ()
prop_randomHistory runner = do
    genHistory runner
    -- | (CreateHistory - DeleteHistory) = crowdSize (Project)
    (creat, remov) <- run $ runner $
        partition ((== Create) . Model.pledgeHistoryAction . entityVal)
            <$> selectList [] []
    crowd <- run $ runner $ projectCrowd <$> fetchProject
    monitor (badSize (creat, remov, crowd))
    assert (length creat - length remov == crowd)

    -- | Pledge exists -> PaymentToken exists
    ct <- run . runner $
        count [ Model.PatronPledgeSince !=. Nothing
              , Model.PatronPaymentToken ==. Nothing]
    assert (ct == 0)
  where
    badSize (c, r, crwd) = counterexample (concat
        [ "bad pledge history: "
        , "create ("
        , show (length c)
        , ") - remove ("
        , show (length r)
        , ") = # pledges ("
        , show crwd
        , ")"
        ])
