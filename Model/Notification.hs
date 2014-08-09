module Model.Notification
    ( archiveNotificationDB
    , sendNotificationDB
    , sendNotificationDB_
    , module Model.Notification.Internal
    ) where

import Import

import Model.Notification.Internal

import Control.Monad.Writer.Strict (tell)

-- | Archive a notification.
archiveNotificationDB :: NotificationId -> DB ()
archiveNotificationDB notif_id =
    update $ \n -> do
    set n [NotificationArchived =. val True]
    where_ (n ^. NotificationId ==. val notif_id)

-- | Send a notification to a user.
sendNotificationDB :: NotificationType -> UserId -> Maybe ProjectId -> Markdown -> SDB NotificationId
sendNotificationDB notif_type user_id mproject_id content = do
    now <- liftIO getCurrentTime
    let notif = Notification now notif_type user_id mproject_id content False
    notif_id <- lift (insert notif)
    tell [ENotificationSent notif_id notif]
    return notif_id

sendNotificationDB_ :: NotificationType -> UserId -> Maybe ProjectId -> Markdown -> SDB ()
sendNotificationDB_ notif_type user_id mproject_id content = void (sendNotificationDB notif_type user_id mproject_id content)
