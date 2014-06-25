module Handler.HonorPledge where

import Import

import Model.User (establishUser, isCurUserEligibleEstablish)

getHonorPledgeR :: Handler Html
getHonorPledgeR = do
    is_elig <- isCurUserEligibleEstablish
    defaultLayout $ do
        setTitle "Honor Pledge | Snowdrift.coop"
        $(widgetFile "honor-pledge")

postHonorPledgeR :: Handler Html
postHonorPledgeR = maybeAuth >>= \case
    Nothing  -> redirect HomeR
    Just (Entity user_id User{..}) ->
        case userEstablished of
            EstEligible elig_time reason -> do
                runDB $ establishUser user_id elig_time reason
                setMessage "Established!"
                redirect HonorPledgeR
            _ -> redirect HonorPledgeR
