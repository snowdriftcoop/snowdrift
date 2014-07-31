module Model.Message.Internal where

import Prelude

import Database.Persist.TH
import Data.Text (Text)

data MessageType
    = MessageDirect     -- Direct message (can't be ignored)
    | MessageBalanceLow -- Balance low (can't be ignored)
    | MessageReply      -- Reply to a comment made.
    | MessageNewProject
    -- Project scope
    | MessageNewPledger
    | MessageNewPage
    deriving (Eq, Read, Show)
derivePersistField "MessageType"

showMessageType :: MessageType -> Text
showMessageType MessageDirect     = "Snowdrift direct messages"
showMessageType MessageBalanceLow = "Balance low"
showMessageType MessageReply      = "Replies to my comments"
showMessageType MessageNewProject = "New project sign-ups"
showMessageType MessageNewPledger = "New pledgers"
showMessageType MessageNewPage    = "New Wiki pages"

data MessageDelivery
    = DeliverInternal     -- Only send internal Snowdrift messages.
    | DeliverEmail        -- Send email in addition to internal messages.
    | DeliverEmailDigest  -- Send email digest in addition to internal messages (sent immediately)
    deriving (Read, Show)
derivePersistField "MessageDelivery"
