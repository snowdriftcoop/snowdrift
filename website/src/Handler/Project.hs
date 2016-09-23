module Handler.Project (getSnowdriftProjectR) where

import Import

import Handler.Pledge (pledgeDeleteForm)
import Handler.TH
import qualified Model.Skeleton as Skeleton

-- | For MVP, there is one, hard-coded project: Snowdrift
getSnowdriftProjectR :: Handler Html
getSnowdriftProjectR = do
    muid <- fmap entityKey <$> maybeAuth
    (donationHistory, projectCrowdSize, userPledge)
        <- runDB $ do
            dh <- Skeleton.projectDonationHistory
            crowd <- count ([] :: [Filter Pledge])
            mpledge <- maybe (pure Nothing) (getBy . UniquePledge) muid
            return (dh, crowd, mpledge)
    let pledgeAmount :: Double = 0.01 * fromIntegral projectCrowdSize
    deletePledgeWidget <- maybe (pure "") (const genDeleteWidget) userPledge
    $(widget "page/snowdrift-project" "Snowdrift.coop Project")
  where
    genDeleteWidget = fst <$> generateFormPost pledgeDeleteForm
