module Handler.Pledge
        ( postPledgeSnowdriftR
        , deletePledgeSnowdriftR
        , pledgeDeleteForm
        , pledgeForm
        ) where

import Import

import Alerts
import Handler.Util
import Crowdmatch

postPledgeSnowdriftR :: Handler Html
postPledgeSnowdriftR = handleDelete' $ do
    Entity uid _ <- requireAuth
    patron <- runDB $ fetchPatron uid
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
        runDB $ storePledge uid
        alertSuccess "You are now pledged!"
    handleDelete' = handleDelete pledgeDeleteFormId deletePledgeSnowdriftR

pledgeDeleteFormId :: Text
pledgeDeleteFormId = "pledge-delete"

pledgeDeleteForm :: Form ()
pledgeDeleteForm =
    identifyForm pledgeDeleteFormId (renderDivsNoLabels deleteFromPost)

-- | There is no data to submit with a pledge, since there is only one
-- project to pledge to.
pledgeForm :: AForm Handler ()
pledgeForm = mempty

deletePledgeSnowdriftR :: Handler Html
deletePledgeSnowdriftR = do
    Entity uid _ <- requireAuth
    runDB $ do
        patron <- fetchPatron uid
        maybe shrugItOff (unpledge' uid) (patronPledgeSince patron)
    redirect SnowdriftProjectR
  where
    shrugItOff = alertInfo "You had no pledge to remove! Carry on. :)"
    unpledge' uid t = do
        deletePledge uid
        alertInfo
            [shamlet|Your pledge that started on #{show t} is now removed.|]
