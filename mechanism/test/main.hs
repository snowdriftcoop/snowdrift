{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-orphans #-}

import Control.Monad.Reader (ask)
import Data.List (partition)
import Data.Text (Text)
import Database.Persist
import Database.Persist.Sql
import RunPersist
import Test.QuickCheck
import Test.QuickCheck.Monadic
import qualified Data.Text as T

import Crowdmatch

instance Arbitrary Text where
    arbitrary = T.pack <$> listOf (elements ['a'..'z'])

instance Arbitrary PPtr where
    arbitrary = PPtr . getPositive <$> arbitrary

instance Arbitrary PaymentToken where
    arbitrary = PaymentToken <$> arbitrary

instance Arbitrary (MechAction ()) where
    arbitrary = oneof
        [ ActStoreStripeCustomer <$> arbitrary <*> arbitrary
        , ActDeleteStripeCustomer <$> arbitrary
        , ActDeletePledge <$> arbitrary
        ]

instance Show (MechAction b) where
    show (ActStoreStripeCustomer p c) = show2 "ActStoreStripeCustomer " p c
    show (ActDeleteStripeCustomer p) = show1 "ActDeleteStripeCustomer" p
    show (ActStorePledge p) = show1 "ActStorePledge" p
    show (ActDeletePledge p) = show1 "ActDeletePledge" p
    show ActFetchCrowdCount = "ActFetchCrowdCount"
    show (ActFetchPatron p) = show1 "ActFetchPatron" p

show1 :: Show a => String -> a -> String
show1 str a = unwords [str, show a]

show2 :: (Show a, Show b) => String -> a -> b -> String
show2 str a b = unwords [str, show a, show b]

main :: IO ()
main = do
    trunq <- runPersist $ do
        runMigration migrateCrowdmatch
        buildTruncQuery
    quickCheck $ do
        targets <- map PPtr <$> listOf (choose (1,1000))
        acts <- traverse oneAct targets
        pure
            $ label (show (length acts `div` 5))
            $ prop_pledgeHist trunq acts
  where
    buildTruncQuery = do
        esc <- connEscapeName <$> ask
        tables <- map (esc . DBName . unSingle)
            <$> rawSql "select table_name from information_schema.tables where table_schema = 'public'" []
        let trunq = "truncate table " `mappend` T.intercalate ", " tables
        -- Don't do it yet
        pure (rawExecute trunq [])
    oneAct x = frequency
        [ (10, ActStoreStripeCustomer <$> pure x <*> arbitrary)
        , (1, pure (ActDeleteStripeCustomer x))
        , (1, pure (ActStorePledge x))
        , (1, pure (ActDeletePledge x))
        ]


propDB :: SqlPersistT IO a -> PropertyM IO a
propDB = run . runPersist

-- (PledgeHistory - DeleteHistory) = crowdSize (FetchCrowd)
prop_pledgeHist truncateTables acts = monadicIO $ do
    p@(creat, remov, crowd) <- propDB $ do
        _ <- truncateTables
        mapM_ runMech acts
        (creat, remov) <-
            partition ((== CreatePledge) . pledgeHistoryAction . entityVal)
                <$> selectList [] []
        crowd <- fetchCrowdCount
        pure (creat, remov, crowd)
    monitor (badSize p)
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
    _types = acts :: [MechAction ()]

-- Properties:
--     Nobody is ever charged a fee of >10% (max fee = 10%)
--     FetchPatron oeualways succeeds
--     Pledge exists -> StripeCustomer exists
