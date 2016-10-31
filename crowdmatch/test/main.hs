{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-orphans -fno-warn-missing-fields #-}

import Control.Exception hiding (assert)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Operational
import Control.Monad.Reader (ask)
import Data.List (partition)
import Data.Monoid
import Data.Text (Text)
import Database.Persist
import Database.Persist.Postgresql
import RunPersist
import System.Directory
import System.Environment
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Web.Stripe.Customer
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.Types as PG

import Crowdmatch

-- | Execute a transaction-free statement in Postgres
pgExecute_ q = bracket open' PG.close (void . (`PG.execute_` q))
  where
    open' = PG.connectPostgreSQL ""

instance Arbitrary Text where
    arbitrary = T.pack <$> listOf (elements ['a'..'z'])

instance Arbitrary PPtr where
    arbitrary = PPtr . getPositive <$> arbitrary

instance Arbitrary PaymentToken where
    arbitrary = fmap (PaymentToken . CustomerId) arbitrary

-- | Use this instead of actually talking to Stripe during tests
dummyStripe :: MonadIO m => StripeT m a -> m (Either b a)
dummyStripe = eval <=< viewT
  where
    eval = \case
        Return x -> pure (Right x)
        CreateCustomerI _tok :>>= k ->
            dummyStripe (k Customer { customerId = CustomerId "dummy" })
        UpdateCustomerI _tok cust :>>= k ->
            dummyStripe (k Customer { customerId = cust })
        DeleteCustomerI _ :>>= k -> dummyStripe (k ())

type Runner = forall a. SqlPersistT IO a -> IO a

-- | A QuickCheck property that uses database actions
dbProp
    :: SqlPersistT IO () -> Runner -> (Runner -> PropertyM IO ()) -> Property
dbProp truncation runner prop' =
    monadicIO (run (runner truncation) >> prop' runner)

-- | Generate Arbitrary crowdmatch history
genHistory :: Runner -> PropertyM IO ()
genHistory runner = do
    targets <- pick (map PPtr <$> listOf (choose (1,1000)))
    let bucket = length targets `div` 5
        b0 = show (bucket * 5)
        b1 = show ((bucket + 1) * 5)
    monitor (label (concat ["N ∈ [", b0,", ",b1 , "]"]))
    acts <- pick (traverse oneAct targets)
    -- Running this concurrently causes "impossible" conflicts during
    -- upsert! This needs to be fixed! But on the threat matrix, it is both
    -- unlikely to happen and has minimal side effects (since the DB stays
    -- consistent, after all.) So I'm punting for now.
    {- run (void (mapConcurrently (runMech runner) acts))-}
    run (mapM_ (runMech runner . getBlind) acts)
  where
    -- Tip: Move this into PropertyM to continue property testing once
    -- the API actions have non-homogeneous return types.
    --
    -- Use Blind rather than write a Show instance for MechAction ().
    oneAct x = Blind <$> frequency
        [ (10, ActStoreStripeCustomer dummyStripe <$> pure x <*> arbitrary)
        , (1, pure (ActDeleteStripeCustomer dummyStripe x))
        , (2, pure (ActStorePledge x))
        , (1, pure (ActDeletePledge x))
        ]

-- | Something to make a Patron out of
newtype HarnessUser = HarnessUser Int

instance ToMechPatron HarnessUser where
    toMechPatron (HarnessUser i) = i
    fromMechPatron = HarnessUser

-- | Run the tests
main :: IO ()
main = setUpTestDatabase $ runPersistPool $ \runner -> do
    trunq <- runner $ do
        runMigration migrateCrowdmatch
        buildTruncQuery
    hspec $ before_ (runner trunq) $ do
        sanityTests runner
        -- Ignore for developing other tests
        -- propTests runner trunq
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
    specify "stored customer is retrievable" $ do
        let cust = PaymentToken (CustomerId "blah")
        storeStripeCustomer runner dummyStripe (HarnessUser 1) cust
        pat <- fetchPatron runner (HarnessUser 1)
        patronPaymentToken pat `shouldBe` Just cust
    specify "fetchPatron always succeeds" $ do
        p1 <- fetchPatron runner (HarnessUser 1)
        p2 <- fetchPatron runner (HarnessUser 1)
        p1 `shouldBe` p2

propTests :: Runner -> SqlPersistT IO () -> Spec
propTests runner trunq = modifyMaxSuccess (* 2) $ do
    prop "Pledge creations + pledge deletions = crowd size"
        $ dbProp trunq runner prop_pledgeHist
    prop "Pledge exists -> StripeCustomer exists"
        $ dbProp trunq runner prop_pledgeCapability

-- | We set up an in-memory database to run tests Fast Enough™.
setUpTestDatabase :: IO () -> IO ()
setUpTestDatabase = bracket setup' teardown' . const
  where
    db = "snowdrift_test"
    ts = "tests"
    shmLoc = "/dev/shm/snowdrift_test_tablespace"
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

-- | (CreateHistory - DeleteHistory) = crowdSize (FetchCrowd)
prop_pledgeHist :: Runner -> PropertyM IO ()
prop_pledgeHist runner = do
    genHistory runner
    (creat, remov) <- run $ runner $
        partition ((== CreatePledge) . pledgeHistoryAction . entityVal)
            <$> selectList [] []
    crowd <- fetchCrowdCount (run . runner)
    monitor (badSize (creat, remov, crowd))
    assert (length creat - length remov == crowdSize crowd)
  where
    badSize (c, r, crwd) = counterexample (concat
        [ "bad pledge history: "
        , "create ("
        , show (length c)
        , ") - remove ("
        , show (length r)
        , ") = # pledges ("
        , show (crowdSize crwd)
        , ")"
        ])

-- | Pledge exists -> StripeCustomer exists
prop_pledgeCapability :: Runner -> PropertyM IO ()
prop_pledgeCapability runner = do
    genHistory runner
    ct <- run . runner $
        count [PatronPledgeSince !=. Nothing, PatronPaymentToken ==. Nothing]
    assert (ct == 0)
