module Handler.Dashboard (getDashboardR) where

import Import

import Handler.TH

data DashboardModel = DashboardModel
        { donationHistory :: [DonationHistory]
        , mpledge :: Maybe (Entity Pledge)
        , crowdSize :: Int
        , nextDonation :: DonationDay
        }

getDashboardR :: Handler Html
getDashboardR = do
    Entity uid _ <- requireAuth
    DashboardModel {..} <- runDB (do
        donationHistory <- map entityVal <$>
            selectList [DonationHistoryUsr ==. uid] [Asc DonationHistoryDate]
        mpledge <- getBy (UniquePledge uid)
        crowdSize <- count ([] :: [Filter Pledge])
        [nextDonation] <-
            fmap
                (map (_nextDonationDate . entityVal))
                (selectList [] [])
        pure DashboardModel {..})
    $(widget "page/dashboard" "Dashboard")
