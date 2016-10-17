{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-orphans #-}

import Crowdmatch
import Data.List (partition)
import Data.Text (Text)
import Database.Persist
import Database.Persist.Sql
import RunPersist
import Test.QuickCheck
import Test.QuickCheck.Monadic
import qualified Data.Text as T

instance Arbitrary Text where
    arbitrary = T.pack <$> listOf (elements ['a'..'z'])

instance Arbitrary PPtr where
    arbitrary = PPtr <$> arbitrary

instance Arbitrary PaymentToken where
    arbitrary = PaymentToken <$> arbitrary

instance Arbitrary (MechAction ()) where
    arbitrary = oneof
        [ ActStoreStripeCustomer <$> arbitrary <*> arbitrary
        , ActDeleteStripeCustomer <$> arbitrary
        , ActDeletePledge <$> arbitrary
        ]

main :: IO ()
main = quickCheck prop_pledgeHist

runDB :: SqlPersistT IO a -> PropertyM IO a
runDB = run . runPersist


prop_pledgeHist acts = monadicIO $ do
    (creat, remov, crowd) <- runDB $ do
        runMigration migrateCrowdmatch
        mapM_ runMech acts
        (creat, remov) <-
            partition ((== CreatePledge) . pledgeHistoryAction . entityVal)
                <$> selectList [] []
        crowd <- fetchCrowdCount
        pure (creat, remov, crowd)
    assert (length creat - length remov == crowdSize crowd)
  where
    _types = acts :: [MechAction ()]

-- Properties:
--     Nobody is ever charged a fee of >10% (max fee = 10%)
--     FetchPatron oeualways succeeds
--     Successful StorePledge -> crowdSize (Previous FetchCrowd) = crowdSize (Current FetchCrowd)
--     Pledge exists -> StripeCustomer exists
--     (PledgeHistory - DeleteHistory) = crowdSize (FetchCrowd)
