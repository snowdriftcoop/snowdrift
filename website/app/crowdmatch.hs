-- | crowdmatch
--
-- Calculates and records the monthly donation totals for patrons.
--
module Main where

import Import.NoFoundation

import Data.Ratio
import RunPersist

import qualified Model.Skeleton as Skeleton

main :: IO ()
main = runPersist crowdmatch

crowdmatch :: MonadIO m => SqlPersistT m ()
crowdmatch = do
    active <- Skeleton.activePatrons
    let projectValue = fromIntegral (length active)
    today <- utctDay <$> liftIO getCurrentTime
    mapM_
        (recordCrowdmatch (CrowdmatchDay today) (DonationUnits projectValue))
        active

recordCrowdmatch
    :: MonadIO m => CrowdmatchDay -> DonationUnits -> Entity Patron -> SqlPersistT m ()
recordCrowdmatch day amt (Entity pid Patron{..}) = do
    insert_ (CrowdmatchHistory pid day amt)
    void (update pid [PatronDonationPayable +=. amt])
