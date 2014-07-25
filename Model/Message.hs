module Model.Message
    ( insertMessage
    , insertMessage_
    , module Model.Message.Internal
    ) where

import Import
import Model.Message.Internal
import Model.SnowdriftEvent

import Control.Monad.Writer.Strict (tell)

insertMessage :: Message -> SDB MessageId
insertMessage message = do
    message_id <- lift $ insert message
    tell [EMessageSent message_id]
    return message_id

insertMessage_ :: Message -> SDB ()
insertMessage_ = void . insertMessage
