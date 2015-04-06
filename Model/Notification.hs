module Model.Notification
    ( archiveUserNotificationDB
    , archiveProjectNotificationDB
    , sendUserNotificationDB
    , sendProjectNotificationDB
    , sendUserNotificationDB_
    , sendProjectNotificationDB_
    , sendUserNotificationEmailDB
    , sendProjectNotificationEmailDB
    , unarchiveUserNotificationDB
    , unarchiveProjectNotificationDB
    , module Model.Notification.Internal
    ) where

import Import

import Model.Notification.Internal

import Control.Monad.Writer.Strict (tell)
import Data.Maybe (fromJust)

updateUserNotificationArchived :: Bool -> UserNotificationId -> DB ()
updateUserNotificationArchived bool notif_id =
    update $ \n -> do
    set n [UserNotificationArchived =. val bool]
    where_ (n ^. UserNotificationId ==. val notif_id)

updateProjectNotificationArchived :: Bool -> ProjectNotificationId -> DB ()
updateProjectNotificationArchived bool notif_id =
    update $ \n -> do
    set n [ProjectNotificationArchived =. val bool]
    where_ (n ^. ProjectNotificationId ==. val notif_id)

archiveUserNotificationDB :: UserNotificationId -> DB ()
archiveUserNotificationDB = updateUserNotificationArchived True

archiveProjectNotificationDB :: ProjectNotificationId -> DB ()
archiveProjectNotificationDB = updateProjectNotificationArchived True

unarchiveUserNotificationDB :: UserNotificationId -> DB ()
unarchiveUserNotificationDB = updateUserNotificationArchived False

unarchiveProjectNotificationDB :: ProjectNotificationId -> DB ()
unarchiveProjectNotificationDB = updateProjectNotificationArchived False

sendUserNotificationDB :: UserNotificationType -> UserId -> Maybe CommentId
                       -> Markdown -> SDB UserNotificationId
sendUserNotificationDB notif_type user_id mcomment_id content = do
    now <- liftIO getCurrentTime
    let notif = UserNotification now notif_type user_id content False
    notif_id <- lift (insert notif)
    -- Record the fact that we send this notification, so we can
    -- delete it when the comment is approved.
    when (notif_type == NotifUnapprovedComment && isJust mcomment_id) $
        lift $ insert_ $ UnapprovedCommentNotification (fromJust mcomment_id) notif_id
    tell [EUserNotificationSent notif_id notif]
    return notif_id

sendProjectNotificationDB :: ProjectNotificationType -> UserId -> ProjectId
                          -> Markdown -> SDB ProjectNotificationId
sendProjectNotificationDB notif_type user_id project_id content = do
    now <- liftIO getCurrentTime
    let notif = ProjectNotification now notif_type user_id project_id content False
    notif_id <- lift (insert notif)
    tell [EProjectNotificationSent notif_id notif]
    return notif_id

sendUserNotificationDB_ :: UserNotificationType -> UserId -> Maybe CommentId
                        -> Markdown -> SDB ()
sendUserNotificationDB_ notif_type user_id mcomment_id content =
    void $ sendUserNotificationDB notif_type user_id mcomment_id content

sendProjectNotificationDB_ :: ProjectNotificationType -> UserId -> ProjectId
                           -> Markdown -> SDB ()
sendProjectNotificationDB_ notif_type user_id project_id content =
    void $ sendProjectNotificationDB notif_type user_id project_id content

sendUserNotificationEmailDB :: UserNotificationType -> UserId -> Markdown -> DB ()
sendUserNotificationEmailDB notif_type user_id content = do
    now <- liftIO getCurrentTime
    insert_ $ UserNotificationEmail now notif_type user_id content

sendProjectNotificationEmailDB :: ProjectNotificationType -> UserId -> ProjectId
                               -> Markdown -> DB ()
sendProjectNotificationEmailDB notif_type user_id project_id content = do
    now <- liftIO getCurrentTime
    insert_ $ ProjectNotificationEmail now notif_type user_id project_id content
