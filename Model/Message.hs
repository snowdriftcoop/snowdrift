module Model.Message
    ( archiveMessageDB
    , sendAnonymousMessageDB
    , sendNotificationMessageDB
    , sendU2UMessageDB
    , sendU2PMessageDB
    , sendP2UMessageDB
    , sendP2PMessageDB
    , module Model.Message.Internal
    ) where

import Import

import Model.Message.Internal
import Model.Project

import Control.Monad.Writer.Strict (tell)

-- | Archive a message.
archiveMessageDB :: MessageId -> DB ()
archiveMessageDB message_id =
    update $ \m -> do
    set m [MessageArchived =. val True]
    where_ (m ^. MessageId ==. val message_id)

-- | Send an anonymous Message to a Project (as feedback, presumably).
sendAnonymousMessageDB :: ProjectId -> Markdown -> SDB [MessageId]
sendAnonymousMessageDB = sendMessage2P Nothing Nothing

-- | Send a "notification" Message that doesn't come from any particular User or Project.
sendNotificationMessageDB :: MessageType -> UserId -> Markdown -> SDB MessageId
sendNotificationMessageDB message_type to_user = sendMessage message_type Nothing Nothing to_user Nothing

-- | Send User-to-User Message.
sendU2UMessageDB :: UserId -> UserId -> Markdown -> SDB MessageId
sendU2UMessageDB from_user to_user = sendMessage MessageDirect (Just from_user) Nothing to_user Nothing

-- | Send User-to-Project Message.
sendU2PMessageDB :: UserId -> ProjectId -> Markdown -> SDB [MessageId]
sendU2PMessageDB from_user = sendMessage2P (Just from_user) Nothing

-- | Send Project-to-User Message, possibly from an actual User (who is representing the Project).
sendP2UMessageDB :: MessageType -> Maybe UserId -> ProjectId -> UserId -> Markdown -> SDB MessageId
sendP2UMessageDB message_type mfrom_user from_project to_user =
    sendMessage message_type mfrom_user (Just from_project) to_user Nothing

-- | Send Project-to-Project Message, possibly from an actual User (who is representing the Project).
sendP2PMessageDB :: Maybe UserId -> ProjectId -> ProjectId -> Markdown -> SDB [MessageId]
sendP2PMessageDB mfrom_user from_project = sendMessage2P mfrom_user (Just from_project)

-- | Abstract sending a Message to all team members of a Project.
sendMessage2P :: Maybe UserId -> Maybe ProjectId -> ProjectId -> Markdown -> SDB [MessageId]
sendMessage2P mfrom_user mfrom_project to_project content = lift (fetchProjectTeamMembersDB to_project) >>= mapM go
  where
    go :: UserId -> SDB MessageId
    go to_user = sendMessage MessageDirect mfrom_user mfrom_project to_user (Just to_project) content

-- | Abstract sending all types of messages. Unexported.
sendMessage :: MessageType -> Maybe UserId -> Maybe ProjectId -> UserId -> Maybe ProjectId -> Markdown -> SDB MessageId
sendMessage message_type mfrom_user mfrom_project to_user mto_project content = do
    now <- liftIO getCurrentTime
    let message = Message now message_type mfrom_user mfrom_project to_user mto_project content False
    message_id <- lift (insert message)
    tell [EMessageSent message_id message]
    return message_id
