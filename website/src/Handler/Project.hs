module Handler.Project (getSnowdriftProjectR) where

import Import

import Handler.TH
import qualified Model.Skeleton as Skeleton

-- | For MVP, there is one, hard-coded project: Snowdrift
getSnowdriftProjectR :: Handler Html
getSnowdriftProjectR = do
    (donationHistory, nextDonationDate, projectCrowdSize)
        <- runDB $ do
            dh <- Skeleton.projectDonationHistory
            [ndd] <- fmap
                (map (_nextDonationDate . entityVal))
                (selectList [] [])
            crowd <- count ([] :: [Filter Pledge])
            return (dh, ndd, crowd)
    let pledgeAmount :: Double = 0.01 * fromIntegral projectCrowdSize
    $(widget "page/snowdrift-project" "Snowdrift.coop Project")
