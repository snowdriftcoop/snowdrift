module Model.Message.Internal where

import Prelude

import Database.Persist.TH
import Data.Text (Text)

data MessageType
    -- Generic "direct" message (can't be ignored)
    = MessageDirect
    -- Balance low (can't be ignored)
    | MessageBalanceLow
    -- Alert moderators about an unapproved comment.
    -- These messages are auto-deleted when the comment is approved.
    | MessageUnapprovedComment
    -- Reply to a comment made.
    | MessageReply
    -- New projected created on Snowdrift.
    | MessageNewProject
    -- New pledger to a Project.
    | MessageNewPledger
    -- New WikiPage.
    | MessageNewPage
    deriving (Eq, Read, Show)
derivePersistField "MessageType"

showMessageType :: MessageType -> Text
showMessageType MessageDirect            = "Snowdrift direct messages"
showMessageType MessageUnapprovedComment = "Unapproved comments"
showMessageType MessageBalanceLow        = "Balance low"
showMessageType MessageReply             = "Replies to my comments"
showMessageType MessageNewProject        = "New project sign-ups"
showMessageType MessageNewPledger        = "New pledgers"
showMessageType MessageNewPage           = "New Wiki pages"

data MessageDelivery
    = DeliverInternal     -- Only send internal Snowdrift messages.
    | DeliverEmail        -- Send email in addition to internal messages.
    | DeliverEmailDigest  -- Send email digest in addition to internal messages (sent immediately)
    deriving (Read, Show)
derivePersistField "MessageDelivery"

-- | Can this message type be filtered out entirely?
messagePreferenceCanBeNone :: MessageType -> Bool
messagePreferenceCanBeNone MessageDirect            = False
messagePreferenceCanBeNone MessageBalanceLow        = False
messagePreferenceCanBeNone MessageUnapprovedComment = True
messagePreferenceCanBeNone MessageReply             = True
messagePreferenceCanBeNone MessageNewProject        = True
messagePreferenceCanBeNone MessageNewPledger        = True
messagePreferenceCanBeNone MessageNewPage           = True
