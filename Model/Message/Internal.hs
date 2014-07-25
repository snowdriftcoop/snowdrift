module Model.Message.Internal where

import Prelude

import Database.Persist.TH

data MessageType
    = MessageDirect     -- Direct message (can't be ignored)
    | MessageBalanceLow -- Balance low (can't be ignored)
    | MessageReply      -- Reply to a comment made.
    | MessageNewProject
    | MessageNewPledger
    | MessageNewPage
    deriving (Read, Show)
derivePersistField "MessageType"
