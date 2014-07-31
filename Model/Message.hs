module Model.Message
    ( insertMessage
    , insertMessage_
    , module Model.Message.Internal
    ) where

import Import
import Model.Message.Internal
import Model.SnowdriftEvent.Internal

import Control.Monad.Writer.Strict (tell)

insertMessage :: MessageType
              -> Maybe ProjectId
              -> Maybe UserId
              -> Maybe UserId
              -> Markdown
              -> Bool
              -> SDB MessageId
insertMessage message_type mproject_id mfrom mto content is_automated = do
    now <- liftIO getCurrentTime
    let message = Message message_type mproject_id now mfrom mto content is_automated
    message_id <- lift (insert message)
    tell [EMessageSent message_id message]
    return message_id

insertMessage_ :: MessageType
               -> Maybe ProjectId
               -> Maybe UserId
               -> Maybe UserId
               -> Markdown
               -> Bool
               -> SDB ()
insertMessage_ a b c d e f = void $ insertMessage a b c d e f
