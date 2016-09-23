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
    Entity uid User {..} <- requireAuth
    case _userStripeCustomer of
        Nothing -> do
            alertWarning
                "Before making a pledge, set up a payment method below"
            redirect DashboardR
        _ -> do
            dbres <- runDB $ do
                p <- getBy (UniquePledge uid)
                maybe (pledge' uid) feignHorror p
            case dbres of
                Just t -> alertInfo
                    [shamlet|
                        Your pledge that started on #{show t} is still valid.
                    |]
                Nothing -> alertSuccess "You are now pledged!"
            redirect SnowdriftProjectR
  where
    pledge' uid = do
        now <- liftIO getCurrentTime
        insert_ (Pledge uid now)
        insert_ (PledgeHistory uid now CreatePledge)
        pure Nothing
    feignHorror (Entity _ Pledge {..}) = pure (Just _pledgeSince)
    handleDelete' = handleDelete pledgeDeleteFormId deletePledgeSnowdriftR

pledgeDeleteFormId :: Text
pledgeDeleteFormId = "pledge-delete"

pledgeDeleteForm :: Form Bool
pledgeDeleteForm =
    identifyForm pledgeDeleteFormId (renderDivsNoLabels deleteFromPost)

deletePledgeSnowdriftR :: Handler Html
deletePledgeSnowdriftR = do
    Entity uid _ <- requireAuth
    dbres <- runDB $ do
        p <- getBy (UniquePledge uid)
        traverse (unpledge' uid) p
    case dbres of
        Just t -> alertInfo
            [shamlet|Your pledge that started on #{show t} is now removed.|]
        Nothing -> alertInfo "You had no pledge to remove! Carry on. :)"
    redirect SnowdriftProjectR
  where
    unpledge' uid (Entity pid Pledge{..}) = do
        now <- liftIO getCurrentTime
        delete pid
        insert_ (PledgeHistory uid now DeletePledge)
        pure _pledgeSince
