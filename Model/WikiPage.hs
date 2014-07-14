module Model.WikiPage
    ( getAllWikiComments
    ) where

import Import

import Model.Comment (filterComments)
import Model.Project (getProjectPages)

-- | Get the unapproved, new and old Comments on all WikiPages of Project. Takes a
-- UTCTime 'since' to filter comments EARLIER than this time, and a CommentId
-- 'latest_comment_id' to filter comments AFTER this comment (used for paging).
getAllWikiComments :: Maybe UserId -> ProjectId -> CommentId -> UTCTime -> Int64 -> YesodDB App ([Entity Comment], [Entity Comment], [Entity Comment])
getAllWikiComments mviewer_id project_id latest_comment_id since limit_num = do
    pages_ids <- map entityKey <$> getProjectPages project_id
    unapproved_comments <- filt <*> getUnapprovedComments pages_ids
    new_comments        <- filt <*> getNewComments pages_ids latest_comment_id since limit_num
    old_comments        <- filt <*> getOldComments pages_ids since (limit_num - fromIntegral (length new_comments))
    return (unapproved_comments, new_comments, old_comments)
  where
    filt = filterComments mviewer_id project_id

getUnapprovedComments :: [WikiPageId] -> YesodDB App [Entity Comment]
getUnapprovedComments pages_ids = do
    select $
        from $ \(c `InnerJoin` wp) -> do
        on_ (c ^. CommentDiscussion ==. wp ^. WikiPageDiscussion)
        where_ (wp ^. WikiPageId `in_` valList pages_ids &&.
                isNothing (c ^. CommentModeratedTs))
        orderBy [desc (c ^. CommentCreatedTs)]
        return c

getNewComments :: [WikiPageId] -> CommentId -> UTCTime -> Int64 -> YesodDB App [Entity Comment]
getNewComments pages_ids latest_comment_id since limit_num = do
    select $
        from $ \(c `InnerJoin` wp) -> do
        on_ (c ^. CommentDiscussion ==. wp ^. WikiPageDiscussion)
        where_ (wp ^. WikiPageId `in_` valList pages_ids &&.
                c ^. CommentId <=. val latest_comment_id &&.
                c ^. CommentModeratedTs >=. just (val since))
        orderBy [desc (c ^. CommentModeratedTs)]
        limit limit_num
        return c

getOldComments :: [WikiPageId] -> UTCTime -> Int64 -> YesodDB App [Entity Comment]
getOldComments pages_ids since limit_num = do
    select $
        from $ \(c `InnerJoin` wp) -> do
        on_ (c ^. CommentDiscussion ==. wp ^. WikiPageDiscussion)
        where_ (wp ^. WikiPageId `in_` valList pages_ids &&.
                c ^. CommentModeratedTs <. just (val since))
        orderBy [desc (c ^. CommentModeratedTs)]
        limit limit_num
        return c
