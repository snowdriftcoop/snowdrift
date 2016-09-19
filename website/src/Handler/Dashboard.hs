module Handler.Dashboard (getDashboardR) where

import Import

import Handler.TH

data DashboardModel = DashboardModel
        { pledgeHistory :: [PledgeHistory]
        , mpledge :: Maybe (Entity Pledge)
        , crowdSize :: Int
        , nextDonation :: UTCTime
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
    -- | Displays a 'UTCTime' with 1-day resolution relative to the current
    -- month.
    donationDayView t = [shamlet|<time datetime=#{rfc3339Day t}>#{monthDay t}#|]
      where
        -- | Example: "2016-09-19"
        rfc3339Day = formatTime defaultTimeLocale "%Y-%m-%d"
        -- | Example: "September 23"
        monthDay = formatTime defaultTimeLocale "%B %d"
    -- | Pull out the interesting bits
    pledgeProjection :: PledgeHistory -> (UTCTime, PledgeAction)
    pledgeProjection PledgeHistory{..} = (_pledgeHistoryTime, _pledgeHistoryAction)
