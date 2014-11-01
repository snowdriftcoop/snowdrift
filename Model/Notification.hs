module Model.Notification
    ( archiveNotificationDB
    , sendNotificationDB
    , sendNotificationDB_
    , sendNotificationEmailDB
    , module Model.Notification.Internal
    ) where

import Import

import Model.Notification.Internal

import Control.Monad.Writer.Strict (tell)
import Data.Maybe (fromJust)

-- | Archive a notification.
archiveNotificationDB :: NotificationId -> DB ()
archiveNotificationDB notif_id =
    update $ \n -> do
    set n [NotificationArchived =. val True]
    where_ (n ^. NotificationId ==. val notif_id)

-- | Send a notification to a user.
sendNotificationDB :: NotificationType -> UserId -> Maybe ProjectId
                   -> Maybe CommentId -> Markdown -> SDB NotificationId
sendNotificationDB notif_type user_id mproject_id mcomment_id content = do
    now <- liftIO getCurrentTime
    let notif = Notification now notif_type user_id mproject_id content False
    notif_id <- lift (insert notif)
    -- Record the fact that we send this notification, so we can
    -- delete it when the comment is approved.
    when (notif_type == NotifUnapprovedComment && isJust mcomment_id) $
        insert_ $ UnapprovedCommentNotification (fromJust mcomment_id) notif_id
    tell [ENotificationSent notif_id notif]
    return notif_id

sendNotificationDB_ :: NotificationType -> UserId -> Maybe ProjectId
                    -> Maybe CommentId -> Markdown -> SDB ()
sendNotificationDB_ notif_type user_id mproject_id mcomment_id content = void $ sendNotificationDB notif_type user_id mproject_id mcomment_id content

sendNotificationEmailDB :: NotificationType -> UserId -> Maybe ProjectId
                        -> Markdown -> DB ()
sendNotificationEmailDB notif_type user_id mproject_id content = do
    now <- liftIO getCurrentTime
    insert_ $ NotificationEmail now notif_type user_id mproject_id content
