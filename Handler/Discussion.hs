module Handler.Discussion where

import Import

import Model.Comment.Sql
import Model.Discussion

-- | Given a callback that takes a "root comment getter", call the callback with the appropriate
-- "root comment getter", by looking for a "state=open" or "state=closed" GET param.
getDiscussion :: ((DiscussionId -> ExprCommentCond -> DB [Entity Comment]) -> Handler Html) -> Handler Html
getDiscussion callback = lookupGetParam "state" >>= \case
    Just "closed" -> callback fetchDiscussionClosedRootCommentsDB
    -- Not "closed"? Just accept anything else as meaning "open".
    _             -> callback fetchDiscussionRootCommentsDB
