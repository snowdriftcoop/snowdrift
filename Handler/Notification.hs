module Handler.Notification where

import Import

import           Model.Notification
import           Model.Project
import           Model.User
import           Widgets.Time

getNotificationsR :: Handler Html
getNotificationsR = do
    user_id <- requireAuthId
    notifs <- runDB $ do
        userReadNotificationsDB user_id
        fetchUserNotificationsDB user_id
    defaultLayout $ do
        setTitle "Notifications | Snowdrift.coop"
        $(widgetFile "notifications")

getArchivedNotificationsR :: Handler Html
getArchivedNotificationsR = do
    user_id <- requireAuthId
    notifs <- runDB (fetchUserArchivedNotificationsDB user_id)
    defaultLayout $ do
        setTitle "Notifications | Snowdrift.coop"
        $(widgetFile "notifications")

postArchiveNotificationR :: NotificationId -> Handler ()
postArchiveNotificationR notif_id = do
    user_id <- requireAuthId
    runYDB $ do
        notif <- get404 notif_id
        unless (user_id == notificationTo notif) $
            lift (permissionDenied "You can't archive this notification.")
        archiveNotificationDB notif_id
