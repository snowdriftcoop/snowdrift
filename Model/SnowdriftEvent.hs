-- TODO(mitchell): Move this to Model.SnowdriftEvent.Internal for consistency
-- (because it can't import Import)
module Model.SnowdriftEvent where

import Prelude
import Model

data SnowdriftEvent
    = ECommentPosted CommentId Comment   -- comment approved
    | ECommentPending CommentId Comment  -- comment unapproved
    | EMessageSent MessageId
    deriving Show
