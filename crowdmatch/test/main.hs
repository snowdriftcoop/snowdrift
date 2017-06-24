{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-orphans -fno-warn-missing-fields #-}

import Control.Exception.Safe (bracket)
import Control.Monad (void, (<=<))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ask)
import Data.Foldable (traverse_)
import Data.List (partition)
import Data.Monoid ((<>))
import Data.Text (Text)
import Database.Persist
import Database.Persist.Postgresql
        (SqlPersistT, runMigration, connEscapeName, unSingle, rawSql, rawExecute)
import RunPersist (runPersistPool)
import System.Directory (createDirectoryIfMissing)
import System.Environment (setEnv)
import Test.Hspec
        (hspec, before_, Spec, describe, it, shouldBe, shouldNotBe, specify)
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
import Web.Stripe.Balance (BalanceTransaction(..))
import Web.Stripe.Charge (Charge(..))
import Web.Stripe.Customer (Customer(..), CustomerId(..), TokenId(..), customerId)
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.Types as PG

import Crowdmatch
import Crowdmatch.Model (StorageAction(..))
import qualified Crowdmatch.Model as Model

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

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

-- | Use this instead of actually talking to Stripe during tests
dummyStripe :: MonadIO m => StripeI a -> m (Either b a)
dummyStripe = \case
    CreateCustomerI _tok ->
        pure (Right Customer { customerId = CustomerId "dummy" })
    UpdateCustomerI _tok cust ->
        pure (Right Customer { customerId = cust })
    DeleteCustomerI _ ->
        pure (Right ())
    ChargeCustomerI _ _ -> pure (Right Charge{})
    BalanceTransactionI _ -> pure (Right BalanceTransaction{})

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
    -- Running this concurrently causes "impossible" conflicts during
    -- upsert! This needs to be fixed! But on the threat matrix, it is both
    -- unlikely to happen and has minimal side effects (since the DB stays
    -- consistent, after all.) So I'm punting for now.
    traverse_ oneAct targets
  where
    oneAct x =
        -- When new API methods are added, they need to be added here. This is
        -- a problem I don't have a solution to right now.
        (run . getBlind <=< pick) (Blind <$> frequency
            [ (10, storeToken' x)
            , (1, delToken' x)
            , (2, storePledge' x)
            , (1, delPledge' x)
            ])
    storeToken' x = void . storePaymentToken runner dummyStripe x <$> arbitrary
    delToken' = pure . void . deletePaymentToken runner dummyStripe
    storePledge' = pure . storePledge runner
    delPledge' = pure . deletePledge runner

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
        sanityTests runner
        propTests runner trunq
  where
    -- A query that empties all tables, used before each database test.
    buildTruncQuery = do
        esc <- connEscapeName <$> ask
        tables <- fmap
            (map (esc . DBName . unSingle))
            (rawSql
                ("select table_name"
                 <> " from information_schema.tables"
                 <> " where table_schema = 'public'")
                [])
        let trunq = "truncate table " `mappend` T.intercalate ", " tables
        -- Don't do it yet
        pure (rawExecute trunq [])

sanityTests :: Runner -> Spec
sanityTests runner = describe "sanity tests" $ do
    let payTok = PaymentToken (CustomerId "dummy")
        cardTok = TokenId "pumpkin"
        aelfred = HarnessUser 1
    describe "stored token" $ do
        it "is retrievable" $ do
            _ <- storePaymentToken runner dummyStripe aelfred cardTok
            pat <- fetchPatron runner aelfred
            patronPaymentToken pat `shouldBe` Just payTok
        it "disappears" $ do
            _ <- storePaymentToken runner dummyStripe aelfred cardTok
            _ <- deletePaymentToken runner dummyStripe aelfred
            pat <- fetchPatron runner aelfred
            patronPaymentToken pat `shouldBe` Nothing
        it "has history recorded" $ do
            _ <- storePaymentToken runner dummyStripe aelfred cardTok
            _ <- deletePaymentToken runner dummyStripe aelfred
            ls <- runner $ map entityVal <$> selectList [] []
            length ls `shouldBe` 2
            Model.paymentTokenHistoryAction (head ls) `shouldBe` Create
            Model.paymentTokenHistoryAction (last ls) `shouldBe` Delete
    specify "fetchPatron creates patron if it doesn't exist" $ do
        p1 <- fetchPatron runner aelfred
        p2 <- fetchPatron runner aelfred
        p1 `shouldBe` p2
    specify "stored pledge is retrievable" $ do
        _ <- storePaymentToken runner dummyStripe aelfred cardTok
        storePledge runner aelfred
        pat <- fetchPatron runner aelfred
        patronPledgeSince pat `shouldNotBe` Nothing
    specify "deleted pledge is retrievable" $ do
        _ <- storePaymentToken runner dummyStripe aelfred cardTok
        storePledge runner aelfred
        deletePledge runner aelfred
        pat <- fetchPatron runner aelfred
        patronPledgeSince pat `shouldBe` Nothing
    describe "crowd size" $ do
        it "gets bumped by pledging" $ do
            _ <- storePaymentToken runner dummyStripe aelfred cardTok
            storePledge runner aelfred
            p <- fetchProject runner
            projectCrowd p `shouldBe` 1
        it "is not affected by a bad pledge" $ do
            -- No token!
            storePledge runner aelfred
            p <- fetchProject runner
            projectCrowd p `shouldBe` 0
        it "shrinks with removal of payment token" $ do
            _ <- storePaymentToken runner dummyStripe aelfred cardTok
            storePledge runner aelfred
            p1 <- projectCrowd <$> fetchProject runner
            p1 `shouldBe` 1
            _ <- deletePaymentToken runner dummyStripe aelfred
            p2 <- projectCrowd <$> fetchProject runner
            p2 `shouldBe` 0
    specify "10 pledges = 1 cent" $ do
        let mkPledge i = do
                _ <- storePaymentToken runner dummyStripe (HarnessUser i) cardTok
                storePledge runner (HarnessUser i)
        mapM_ mkPledge [1..10]
        val <- projectPledgeValue <$> fetchProject runner
        val `shouldBe` centsToUnits (Cents 1)
    specify "1000 pledges = $1000 monthly income" $ do
        let mkPledge i = do
                _ <- storePaymentToken runner dummyStripe (HarnessUser i) cardTok
                storePledge runner (HarnessUser i)
        mapM_ mkPledge [1..1000]
        val <- projectMonthlyIncome <$> fetchProject runner
        val `shouldBe` Cents (100 * 1000)
    describe "crowdmatch event" $ do
        let mkPatron i =
                void (storePaymentToken runner dummyStripe (HarnessUser i) cardTok)
            mkPledge i = do
                mkPatron i
                storePledge runner (HarnessUser i)
        -- 1. Two patrons, one active -> 0.01 payable from the patron
        it "only counts active patrons" $ do
            mkPatron 1
            mkPledge 2
            crowdmatch runner
            val <- projectDonationReceivable <$> fetchProject runner
            val `shouldBe` DonationUnits 1
        it "looks quadratic; 3 patrons = 9 DUs" $ do
            mapM_ mkPledge [1..3]
            crowdmatch runner
            val <- projectDonationReceivable <$> fetchProject runner
            val `shouldBe` DonationUnits 9
        it "sums over multiple events" $ do
            mkPatron 1
            mkPledge 2
            crowdmatch runner
            storePledge runner (HarnessUser 1)
            -- Now 1 is also active
            crowdmatch runner
            val <- projectDonationReceivable <$> fetchProject runner
            val `shouldBe` DonationUnits 5
        it "creates history" $ do
            mkPledge 1
            crowdmatch runner
            u :: [Entity Model.CrowdmatchHistory] <- runner $ selectList [] []
            length u `shouldBe` 1

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
    setup' = do
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
        setEnv "PGDATABASE" (B.unpack (PG.fromQuery db))
    teardown' = const $ do
        setEnv "PGDATABASE" "template1"
        pgExecute_ ("drop database " <> db)
        pgExecute_ ("drop tablespace " <> ts)

prop_randomHistory :: Runner -> PropertyM IO ()
prop_randomHistory runner = do
    genHistory runner
    -- | (CreateHistory - DeleteHistory) = crowdSize (Project)
    (creat, remov) <- run $ runner $
        partition ((== Create) . Model.pledgeHistoryAction . entityVal)
            <$> selectList [] []
    crowd <- projectCrowd <$> fetchProject (run . runner)
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
