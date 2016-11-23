module Handler.Pledge
        ( postPledgeSnowdriftR
        , deletePledgeSnowdriftR
        , pledgeDeleteForm
        ) where

import Import

import Alerts
import Handler.Util
import Crowdmatch

postPledgeSnowdriftR :: Handler Html
postPledgeSnowdriftR = handleDelete' $ do
    Entity uid _ <- requireAuth
    patron <- fetchPatron runDB uid
    maybe
        (do
            alertWarning "Before making a pledge, set up a payment method below"
            redirect DashboardR)
        (const $ do
            -- Nothing == "no problem" in this case
            maybe (pledge' uid) showExisting (patronPledgeSince patron)
            redirect SnowdriftProjectR)
        (patronPaymentToken patron)
  where
    showExisting t = alertInfo
        [shamlet|
            Your pledge that started on #{show t} is still valid.
        |]
    pledge' uid = do
        storePledge runDB uid
        alertSuccess "You are now pledged!"
    handleDelete' = handleDelete pledgeDeleteFormId deletePledgeSnowdriftR

pledgeDeleteFormId :: Text
pledgeDeleteFormId = "pledge-delete"

pledgeDeleteForm :: Form Bool
pledgeDeleteForm =
    identifyForm pledgeDeleteFormId (renderDivsNoLabels deleteFromPost)

deletePledgeSnowdriftR :: Handler Html
deletePledgeSnowdriftR = do
    Entity uid _ <- requireAuth
    patron <- fetchPatron runDB uid
    maybe shrugItOff (unpledge' uid) (patronPledgeSince patron)
    redirect SnowdriftProjectR
  where
    shrugItOff = alertInfo "You had no pledge to remove! Carry on. :)"
    unpledge' uid t = do
        deletePledge runDB uid
        alertInfo
            [shamlet|Your pledge that started on #{show t} is now removed.|]
