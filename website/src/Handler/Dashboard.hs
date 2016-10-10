module Handler.Dashboard (getDashboardR) where

import Import

import Handler.TH

data DashboardModel = DashboardModel
        { donationHistory :: [DonationHistory]
        , mpledgeSince :: Maybe UTCTime
        , crowdSize :: Int
        }

getDashboardR :: Handler Html
getDashboardR = do
    Entity uid User{..} <- requireAuth
    Entity pid Patron {..} <- runDB (fetchUserPatron uid)
    DashboardModel {..} <- runDB (do
        donationHistory <- map entityVal <$>
            selectList [DonationHistoryPatron ==. pid] [Asc DonationHistoryTime]
        let mpledgeSince = _patronPledgeSince
        crowdSize <- count [PatronPledgeSince /<-. [Nothing]]
        pure DashboardModel {..})
    let pledgeAmount :: Double = 0.001 * fromIntegral crowdSize
    $(widget "page/dashboard" "Dashboard")
