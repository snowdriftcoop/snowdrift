module Handler.Dashboard (getDashboardR) where

import Import

import Handler.TH

data DashboardModel = DashboardModel
        { pledgeHistory :: [PledgeHistory]
        , mpledge :: Maybe (Entity Pledge)
        , crowdSize :: Int
        , nextDonation :: DonationDay
        }

getDashboardR :: Handler Html
getDashboardR = do
    Entity uid _ <- requireAuth
    DashboardModel {..} <- runDB (do
        pledgeHistory <- map entityVal <$>
            selectList [PledgeHistoryUsr ==. uid] [Asc PledgeHistoryTime]
        mpledge <- getBy (UniquePledge uid)
        crowdSize <- count ([] :: [Filter Pledge])
        [nextDonation] <-
            fmap
                (map (_nextDonationDate . entityVal))
                (selectList [] [])
        pure DashboardModel {..})
    $(widget "page/dashboard" "Dashboard")
  where
    -- | Pull out the interesting bits
    pledgeProjection :: PledgeHistory -> (UTCTime, PledgeAction)
    pledgeProjection PledgeHistory{..} = (_pledgeHistoryTime, _pledgeHistoryAction)
