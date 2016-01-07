module Handler.Discussion where

import Import

import Model.Comment.Sql
import Model.Discussion

-- | Given a callback that takes a "root comment getter", call the callback with the appropriate
-- "root comment getter", by looking for a "state=open" or "state=closed" GET param.
getDiscussion
    :: Maybe Text
    -> ((DiscussionId -> ExprCommentCond -> DB [Entity Comment]) -> Handler Html)
    -> Handler Html
getDiscussion closedView callback =
    case closedView of
        Just "closed" -> callback fetchDiscussionClosedOrRetractedRootCommentsDB
        -- Not "closed"? Just accept anything else as meaning "open".
        _             -> callback fetchDiscussionRootCommentsDB
