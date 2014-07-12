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
postHonorPledgeR = do
    Entity user_id user <- requireAuth
    case userEstablished user of
        EstEligible elig_time reason -> do
            runDB $ establishUser user_id elig_time reason
            setMessage "Congratulations, you are now a fully established user!"
            redirect HonorPledgeR
        _ -> error "You're not eligible for establishment."
