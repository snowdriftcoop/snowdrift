module Handler.Pledge
        ( postPledgeSnowdriftR
        , deletePledgeSnowdriftR
        , pledgeDeleteForm
        ) where

import Import

import Alerts
import Handler.Util

postPledgeSnowdriftR :: Handler Html
postPledgeSnowdriftR = handleDelete' $ do
    Entity uid User{..} <- requireAuth
    Entity pid Patron{..} <- runDB (fetchUserPatron uid)
    maybe
        (do
            alertWarning "Before making a pledge, set up a payment method below"
            redirect DashboardR)
        (const $ do
            -- Nothing == "no problem" in this case
            maybe (pledge' pid) showExisting _patronPledgeSince
            redirect SnowdriftProjectR)
        _patronStripeCustomer
  where
    showExisting t = alertInfo
        [shamlet|
            Your pledge that started on #{show t} is still valid.
        |]
    pledge' pid = do
        runDB $ do
            now <- liftIO getCurrentTime
            update pid [PatronPledgeSince =. Just now]
            insert_ (PledgeHistory pid now CreatePledge)
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
    Entity pid Patron{..} <- runDB (fetchUserPatron uid)
    maybe shrugItOff (unpledge' pid) _patronPledgeSince
    redirect SnowdriftProjectR
  where
    shrugItOff = alertInfo "You had no pledge to remove! Carry on. :)"
    unpledge' pid t = do
        runDB $ do
            now <- liftIO getCurrentTime
            update pid [PatronPledgeSince =. Nothing]
            insert_ (PledgeHistory pid now DeletePledge)
        alertInfo
            [shamlet|Your pledge that started on #{show t} is now removed.|]
