module Model.WikiPage
    ( getAllWikiComments
    ) where

import Import

import Model.Comment.Sql
import Model.Project     (getProjectPages)

-- | Get the unapproved, new and old Comments on all WikiPages of Project. Takes a
-- UTCTime 'since' to filter comments EARLIER than this time, and a CommentId
-- 'latest_comment_id' to filter comments AFTER this comment (used for paging).
getAllWikiComments :: Maybe UserId -> ProjectId -> CommentId -> UTCTime -> Int64 -> DB ([Entity Comment], [Entity Comment], [Entity Comment])
getAllWikiComments mviewer_id project_id latest_comment_id since limit_num = do
    pages_ids           <- map entityKey <$> getProjectPages project_id
    unapproved_comments <- getUnapprovedComments pages_ids
    new_comments        <- getNewComments        pages_ids
    old_comments        <- getOldComments        pages_ids (limit_num - fromIntegral (length new_comments))
    return (unapproved_comments, new_comments, old_comments)
  where
    getUnapprovedComments :: [WikiPageId] -> DB [Entity Comment]
    getUnapprovedComments pages_ids =
        select $
        from $ \(c `InnerJoin` wp) -> do
        on_ (c ^. CommentDiscussion ==. wp ^. WikiPageDiscussion)
        where_ $
            wp ^. WikiPageId `in_` valList pages_ids &&.
            exprUnapproved c &&.
            exprPermissionFilter mviewer_id (val project_id) c
        orderBy [desc (c ^. CommentCreatedTs)]
        return c

    getNewComments :: [WikiPageId] -> DB [Entity Comment]
    getNewComments pages_ids =
        select $
        from $ \(c `InnerJoin` wp) -> do
        on_ (c ^. CommentDiscussion ==. wp ^. WikiPageDiscussion)
        where_ $
            wp ^. WikiPageId `in_` valList pages_ids &&.
            c ^. CommentId <=. val latest_comment_id &&.
            c ^. CommentModeratedTs >=. just (val since) &&.
            exprPermissionFilter mviewer_id (val project_id) c
        orderBy [desc (c ^. CommentModeratedTs)]
        limit limit_num
        return c

    getOldComments :: [WikiPageId] -> Int64 -> DB [Entity Comment]
    getOldComments pages_ids lim =
        select $
        from $ \(c `InnerJoin` wp) -> do
        on_ (c ^. CommentDiscussion ==. wp ^. WikiPageDiscussion)
        where_ $
            wp ^. WikiPageId `in_` valList pages_ids &&.
            c ^. CommentModeratedTs <. just (val since) &&.
            exprPermissionFilter mviewer_id (val project_id) c
        orderBy [desc (c ^. CommentModeratedTs)]
        limit lim
        return c
