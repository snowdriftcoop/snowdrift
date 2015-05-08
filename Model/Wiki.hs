module Model.Wiki
    ( createWikiEditDB
    , createWikiPageDB
    , createWikiTranslationDB
    , fetchWikiPageTargetsInDB
    , getAllWikiComments
    ) where

import Import

import Model.Comment.Sql
import Model.Discussion
import Model.Permission
import Model.Project               (getProjectPages)

import Control.Monad.Writer.Strict (tell)

createWikiPageDB
    :: Language
    -> Text
    -> ProjectId
    -> Markdown
    -> PermissionLevel
    -> UserId
    -> SDB ()
createWikiPageDB
        language
        target
        project_id
        content
        permission_level
        user_id = do

    now           <- liftIO getCurrentTime
    discussion_id <- lift createDiscussionDB

    let wiki_page =
            WikiPage now user_id project_id discussion_id permission_level
    wiki_page_id <- lift $ insert wiki_page

    let wiki_target = WikiTarget wiki_page_id project_id target language
    lift $ insert_ wiki_target

    -- Don't generate a WikiEdit event in addition to this WikiPage event.
    wiki_edit_id <-
        lift $
            insert $
                WikiEdit now
                         user_id
                         wiki_page_id
                         language
                         content
                         (Just "Page created.")
    lift $ insert_ $ WikiLastEdit wiki_page_id wiki_edit_id language
    tell [EWikiPage wiki_page_id wiki_page wiki_target]

createWikiTranslationDB
    :: WikiPageId
    -> Language
    -> Text
    -> ProjectId
    -> Markdown
    -> UserId
    -> [(WikiEditId, Bool)]
    -> SDB ()
createWikiTranslationDB
        wiki_page_id
        language
        target
        project_id
        content
        user_id
        sources = do

    now           <- liftIO getCurrentTime

    let wiki_target = WikiTarget wiki_page_id project_id target language
    lift $ insert_ wiki_target

    -- Don't generate a WikiEdit event in addition to this WikiPage event.
    let wiki_edit =
            WikiEdit now
                     user_id
                     wiki_page_id
                     language
                     content
                     (Just "Translation created.")
    wiki_edit_id <- lift $ insert wiki_edit

    lift $ insert_ $ WikiLastEdit wiki_page_id wiki_edit_id language

    forM_ sources $ \ (source_edit_id, complete) ->
        lift $ insert_ $ WikiTranslation wiki_edit_id source_edit_id complete

    tell [EWikiEdit wiki_edit_id wiki_edit wiki_target]


createWikiEditDB
    :: UserId
    -> WikiPageId
    -> Language
    -> Markdown
    -> Maybe Text
    -> SDB WikiEditId
createWikiEditDB user_id wiki_page_id language content mcomment = do
    now <- liftIO getCurrentTime
    let wiki_edit = WikiEdit now user_id wiki_page_id language content mcomment
    wiki_edit_id <- lift (insert wiki_edit)

    -- TODO - pick this better
    [ Entity _ wiki_target ] <- lift $ select $ from $ \ wt -> do
        where_ $ wt ^. WikiTargetPage ==. val wiki_page_id
        limit 1
        return wt

    tell [EWikiEdit wiki_edit_id wiki_edit wiki_target]
    return wiki_edit_id

fetchWikiPageTargetsInDB :: [WikiPageId] -> DB [Entity WikiTarget]
fetchWikiPageTargetsInDB wiki_page_ids = select $ from $ \ wt -> do
    where_ $ wt ^. WikiTargetPage `in_` valList wiki_page_ids
    return wt

-- | Get the unapproved, new and old Comments on all WikiPages of Project.
-- Takes a UTCTime 'since' to filter comments EARLIER than this time, and a
-- CommentId 'latest_comment_id' to filter comments AFTER this comment
-- (used for paging).
getAllWikiComments
    :: Maybe UserId
    -> ProjectId
    -> CommentId
    -> UTCTime
    -> Int64
    -> DB ([Entity Comment], [Entity Comment], [Entity Comment])
getAllWikiComments mviewer_id project_id latest_comment_id since limit_num = do
    pages_ids           <- map entityKey <$> getProjectPages project_id
    unapproved_comments <- getUnapprovedComments pages_ids
    new_comments        <- getNewComments        pages_ids
    old_comments        <-
        getOldComments pages_ids
                       (limit_num - fromIntegral (length new_comments))
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
            c ^. CommentApprovedTs >=. just (val since) &&.
            exprCommentProjectPermissionFilter mviewer_id (val project_id) c
        orderBy [desc (c ^. CommentApprovedTs)]
        limit limit_num
        return c

    getOldComments :: [WikiPageId] -> Int64 -> DB [Entity Comment]
    getOldComments pages_ids lim =
        select $
        from $ \(c `InnerJoin` wp) -> do
        on_ (c ^. CommentDiscussion ==. wp ^. WikiPageDiscussion)
        where_ $
            wp ^. WikiPageId `in_` valList pages_ids &&.
            c ^. CommentApprovedTs <. just (val since) &&.
            exprCommentProjectPermissionFilter mviewer_id (val project_id) c
        orderBy [desc (c ^. CommentApprovedTs)]
        limit lim
        return c
