module Handler.User.Notifications where

import Import

import Handler.Utils
import Handler.User.Utils (checkEditUser)
import Model.User
import View.User

-- /#UserId/user-notifications

getUserNotificationsR :: UserId -> Handler Html
getUserNotificationsR user_id = do
    void $ checkEditUser user_id
    user <- runYDB $ get404 user_id
    let fetchNotifPref = runYDB . fetchUserNotificationPrefDB user_id
    mbal   <- fetchNotifPref NotifBalanceLow
    mucom  <- fetchNotifPref NotifUnapprovedComment
    mrcom  <- fetchNotifPref NotifRethreadedComment
    mrep   <- fetchNotifPref NotifReply
    mecon  <- fetchNotifPref NotifEditConflict
    mflag  <- fetchNotifPref NotifFlag
    mflagr <- fetchNotifPref NotifFlagRepost
    is_moderator    <- runDB $ userIsModerator user_id
    (form, enctype) <- generateFormPost $
        userNotificationsForm is_moderator
            mbal mucom mrcom mrep mecon mflag mflagr
    defaultLayout $ do
        snowdriftDashTitle "Notification Preferences" $
            userDisplayName (Entity user_id user)
        $(widgetFile "user_notifications")

postUserNotificationsR :: UserId -> Handler Html
postUserNotificationsR user_id = do
    void $ checkEditUser user_id
    is_moderator <- runDB $ userIsModerator user_id
    ((result, form), enctype) <- runFormPost $
        userNotificationsForm is_moderator
            Nothing Nothing Nothing Nothing Nothing Nothing Nothing
    case result of
        FormSuccess notif_pref -> do
            forM_ (userNotificationPref notif_pref) $ \(ntype, ndeliv) ->
                runDB $ updateUserNotificationPrefDB user_id ntype ndeliv
            alertSuccess "Successfully updated the notification preferences."
            redirect $ UserR user_id
        _ -> do
            alertDanger $ "Failed to update the notification preferences. "
                       <> "Please try again."
            defaultLayout $(widgetFile "user_notifications")
