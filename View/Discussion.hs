module View.Discussion where

import Model.Comment.Internal

discussionCommentTree :: CommentMods              -- ^ Comment structure modifications.
                      -> CommentActionPermissions
                      -> MaxDepth
                      -> Bool           -- ^ Is preview?
                      -> Widget         -- ^ Widget to display under root comment.
                      -> CommentId      -- ^ Root comment id.
                      -> Handler (Widget, Tree (Entity Comment))
