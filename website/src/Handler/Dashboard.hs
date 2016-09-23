module Handler.Dashboard (getDashboardR) where

import Import

import Handler.TH

data DashboardModel = DashboardModel
        { donationHistory :: [DonationHistory]
        , mpledge :: Maybe (Entity Pledge)
        , crowdSize :: Int
        }

getDashboardR :: Handler Html
getDashboardR = do
    Entity uid _ <- requireAuth
    DashboardModel {..} <- runDB (do
        donationHistory <- map entityVal <$>
            selectList [DonationHistoryUsr ==. uid] [Asc DonationHistoryDate]
        mpledge <- getBy (UniquePledge uid)
        crowdSize <- count ([] :: [Filter Pledge])
        pure DashboardModel {..})
    let pledgeAmount :: Double = 0.01 * fromIntegral crowdSize
    $(widget "page/dashboard" "Dashboard")
