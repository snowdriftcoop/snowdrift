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

getNotificationsProxyR :: Handler Html
getNotificationsProxyR = do
    user_id <- requireAuthId
    req <- getRequest
    let params  = reqGetParams req
        names   = fst `map` params
        archive = "archive" `elem` names
        delete  = "delete"  `elem` names
        handleAction archive_action delete_action =
            if | archive   -> archive_action
               | delete    -> delete_action
               | otherwise -> return ()
    forM_ params $ \(name, value) ->
        if | name == "all" ->
                 handleAction (runDB $ archiveNotificationsDB user_id)
                              (runDB $ deleteNotificationsDB user_id)
           | name == "notification" ->
                 whenNotifId value $ \notif_id ->
                     handleAction (runDB $ archiveNotificationDB notif_id)
                                  (runDB $ deleteNotificationDB notif_id)
           | otherwise -> return ()
    redirect NotificationsR

getArchivedNotificationsR :: Handler Html
getArchivedNotificationsR = do
    user_id <- requireAuthId
    notifs <- runDB (fetchUserArchivedNotificationsDB user_id)
    defaultLayout $ do
        setTitle "Archived Notifications | Snowdrift.coop"
        $(widgetFile "archived_notifications")

getArchivedNotificationsProxyR :: Handler Html
getArchivedNotificationsProxyR = do
    user_id <- requireAuthId
    req <- getRequest
    let params    = reqGetParams req
        names     = fst `map` params
        unarchive = "unarchive" `elem` names
        delete    = "delete"    `elem` names
        handleAction unarchive_action delete_action =
            if | unarchive -> unarchive_action
               | delete    -> delete_action
               | otherwise -> return ()
    forM_ params $ \(name, value) ->
        if | name == "all" ->
                 handleAction (runDB $ unarchiveNotificationsDB user_id)
                              (runDB $ deleteArchivedNotificationsDB user_id)
           | name == "notification" ->
                 whenNotifId value $ \notif_id ->
                     handleAction (runDB $ unarchiveNotificationDB notif_id)
                                  (runDB $ deleteNotificationDB notif_id)
           | otherwise -> return ()
    redirect ArchivedNotificationsR
