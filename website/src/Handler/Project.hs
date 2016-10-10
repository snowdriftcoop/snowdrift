module Handler.Project (getSnowdriftProjectR) where

import Import

import Handler.Pledge (pledgeDeleteForm)
import Handler.TH
import qualified Model.Skeleton as Skeleton

-- | For MVP, there is one, hard-coded project: Snowdrift
getSnowdriftProjectR :: Handler Html
getSnowdriftProjectR = do
    muid <- fmap entityKey <$> maybeAuth
    mpatron <- fmap entityVal <$> traverse (runDB . fetchUserPatron) muid
    (donationHistory, projectCrowdSize)
        <- runDB $ do
            dh <- Skeleton.projectDonationHistory
            crowd <- count [PatronPledgeSince /<-. [Nothing]]
            return (dh, crowd)
    let pledgeAmount :: Double = 0.001 * fromIntegral projectCrowdSize
    deletePledgeWidget <-
        maybe (pure "") (const genDeleteWidget) (_patronPledgeSince =<< mpatron)
    $(widget "page/snowdrift-project" "Snowdrift.coop Project")
  where
    genDeleteWidget = fst <$> generateFormPost pledgeDeleteForm
