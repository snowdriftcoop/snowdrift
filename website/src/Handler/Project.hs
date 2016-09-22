module Handler.Project (getSnowdriftProjectR) where

import Import

import Control.Lens
import Database.Persist.Sql

import Handler.TH

-- | For MVP, there is one, hard-coded project: Snowdrift
getSnowdriftProjectR :: Handler Html
getSnowdriftProjectR = do
    (donationHistory, nextDonationDate, projectCrowdSize)
        :: ([(Int, DonationDay)], DonationDay, Int)
        <- runDB $ do
            dh <- fmap
                (map ((_1 %~ unSingle) . (_2 %~ unSingle)))
                (rawSql
                     "select sum(amount) \"total\", date from donation_history group by date order by date"
                     [])
            [ndd] <- fmap
                (map (_nextDonationDate . entityVal))
                (selectList [] [])
            crowd <- count ([] :: [Filter Pledge])
            return (dh, ndd, crowd)
    let pledgeAmount :: Double = 0.01 * fromIntegral projectCrowdSize
    $(widget "page/snowdrift-project" "Snowdrift.coop Project")
