module Handler.Notification where

import Import

import           Model.Notification
import           Model.Project
import           Model.User
import qualified Data.Foldable as F
import qualified Data.Text as T
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

getNotificationsProxyR :: Handler Html
getNotificationsProxyR = do
    user_id <- requireAuthId
    req <- getRequest
    let params = reqGetParams req
    forM_ params $ \(name, value) ->
        if | name == "delete_all" ->
                 runDB $ deleteNotificationsDB user_id
           | name == "delete" ->
                 F.forM_ (readMaybe $ T.unpack value :: Maybe Int) $ \notif_id ->
                     runDB $ deleteNotificationDB $ Key $ toPersistValue notif_id
           | otherwise -> return ()
    redirect NotificationsR

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
