module Handler.Notification where

import Import hiding (delete)

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

whenNotifId :: DBConstraint m => Text -> (NotificationId -> m ()) -> m ()
whenNotifId value action =
    F.forM_ (readMaybe $ T.unpack value :: Maybe Int) $ \notif_id ->
        action $ Key $ toPersistValue notif_id

proxyNotifications :: RedirectUrl App route => Text -> Text
                   -> (UserId -> DB ()) -> (UserId -> DB ())
                   -> (NotificationId -> DB ()) -> (NotificationId -> DB ())
                   -> route -> Handler Html
proxyNotifications value1 value2 action_all1 action_all2
                   action_notif1 action_notif2 route = do
    user_id <- requireAuthId
    req <- getRequest
    let params = reqGetParams req
        names  = fst `map` params
        handleAction :: DB () -> DB () -> DB ()
        handleAction action1 action2 =
            if | value1 `elem` names -> action1
               | value2 `elem` names -> action2
               | otherwise -> return ()
    forM_ params $ \(name, value) ->
        if | name == "all" ->
                 runDB $ handleAction (action_all1 user_id)
                                      (action_all2 user_id)
           | name == "notification" ->
                 whenNotifId value $ \notif_id -> runDB $
                     handleAction (action_notif1 notif_id)
                                  (action_notif2 notif_id)
           | otherwise -> return ()
    redirect route

getNotificationsProxyR :: Handler Html
getNotificationsProxyR =
    proxyNotifications "archive" "delete"
        archiveNotificationsDB deleteNotificationsDB
        archiveNotificationDB  deleteNotificationDB
        NotificationsR

getArchivedNotificationsR :: Handler Html
getArchivedNotificationsR = do
    user_id <- requireAuthId
    notifs <- runDB (fetchUserArchivedNotificationsDB user_id)
    defaultLayout $ do
        setTitle "Archived Notifications | Snowdrift.coop"
        $(widgetFile "archived_notifications")

getArchivedNotificationsProxyR :: Handler Html
getArchivedNotificationsProxyR =
    proxyNotifications "unarchive" "delete"
        unarchiveNotificationsDB deleteArchivedNotificationsDB
        unarchiveNotificationDB  deleteNotificationDB
        ArchivedNotificationsR
