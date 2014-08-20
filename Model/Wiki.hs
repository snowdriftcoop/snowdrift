module Model.Wiki
    ( createWikiEditDB
    , createWikiPageDB
    , fetchWikiPagesInDB
    , getAllWikiComments
    ) where

import Import

import Model.Comment.Sql
import Model.Discussion
import Model.Permission
import Model.Project               (getProjectPages)

import Control.Monad.Writer.Strict (tell)

createWikiPageDB :: Text -> ProjectId -> Markdown -> PermissionLevel -> UserId -> SDB ()
createWikiPageDB target project_id content permission_level user_id = do
    now           <- liftIO getCurrentTime
    discussion_id <- lift createDiscussionDB
    let wiki_page = WikiPage now target project_id content discussion_id permission_level
    wiki_page_id <- lift (insert wiki_page)
    -- Don't generate a WikiEdit event in addition to this WikiPage event.
    wiki_edit_id <- lift (insert (WikiEdit now user_id wiki_page_id content (Just "Page created.")))
    lift $ insert_ (WikiLastEdit wiki_page_id wiki_edit_id)
    tell [EWikiPage wiki_page_id wiki_page]

createWikiEditDB :: UserId -> WikiPageId -> Markdown -> Maybe Text -> SDB WikiEditId
createWikiEditDB user_id wiki_page_id content mcomment = do
    now <- liftIO getCurrentTime
    let wiki_edit = WikiEdit now user_id wiki_page_id content mcomment
    wiki_edit_id <- lift (insert wiki_edit)
    tell [EWikiEdit wiki_edit_id wiki_edit]
    return wiki_edit_id

fetchWikiPagesInDB :: [WikiPageId] -> DB [Entity WikiPage]
fetchWikiPagesInDB wiki_page_ids =
    select $
    from $ \wp -> do
    where_ (wp ^. WikiPageId `in_` valList wiki_page_ids)
    return wp

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
            exprCommentUnapproved c &&.
            exprCommentProjectPermissionFilter mviewer_id (val project_id) c
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
            exprCommentProjectPermissionFilter mviewer_id (val project_id) c
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
            exprCommentProjectPermissionFilter mviewer_id (val project_id) c
        orderBy [desc (c ^. CommentModeratedTs)]
        limit lim
        return c
