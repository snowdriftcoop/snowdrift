module Model.Comment
    ( getCommentPageId
    , getCommentTags
    , getRepliesComments
    , getRootComments
    , getTags
    ) where

import Import

getCommentPageId :: CommentId -> YesodDB App WikiPageId
getCommentPageId comment_id = do
    [Value page_id] <-
        select $
            from $ \(c `InnerJoin` p) -> do
            on_ (c ^. CommentDiscussion ==. p ^. WikiPageDiscussion)
            where_ (c ^. CommentId ==. val comment_id)
            return (p ^. WikiPageId)
    return page_id

-- Get a Comment's CommentTags.
getCommentTags :: CommentId -> YesodDB App [Entity CommentTag]
getCommentTags comment_id =
    select $
        from $ \comment_tag -> do
        where_ $ comment_tag ^. CommentTagComment ==. val comment_id
        return comment_tag

getRepliesComments :: Bool -> DiscussionId -> YesodDB App [Entity Comment]
getRepliesComments is_moderator discussion_id =
    select $
        from $ \comment -> do
        where_ $
            genericCommentConditions is_moderator discussion_id comment &&.
            not_ (isNothing $ comment ^. CommentParent)
        orderBy [asc (comment ^. CommentParent), asc (comment ^. CommentCreatedTs)]
        return comment

getRootComments :: Bool -> DiscussionId -> YesodDB App [Entity Comment]
getRootComments is_moderator discussion_id =
    select $
        from $ \comment -> do
        where_ $
            genericCommentConditions is_moderator discussion_id comment &&.
            isNothing (comment ^. CommentParent)
        orderBy [asc (comment ^. CommentCreatedTs)]
        return comment

-- Generic comment conditions that apply to both root comments and child comments.
-- Join on discussion_id, don't display rethreaded comments (they should always
-- be hidden as they've been replaced by another comment), and only show un-moderated
-- comments if the user is a moderator of the page.
genericCommentConditions :: Esqueleto query expr backend
                         => Bool
                         -> DiscussionId
                         -> expr (Entity Comment)
                         -> expr (Value Bool)
genericCommentConditions is_moderator discussion_id comment_table =
    comment_table ^. CommentDiscussion ==. val discussion_id &&.
    isNothing (comment_table ^. CommentRethreaded) &&.
    if is_moderator
        then val True
        else not_ . isNothing $ comment_table ^. CommentModeratedTs

-- Get a Comment's Tags.
getTags :: CommentId -> YesodDB App [Entity Tag]
getTags comment_id =
    select $
        from $ \(comment_tag `InnerJoin` tag) -> do
        on_ (comment_tag ^. CommentTagTag ==. tag ^. TagId)
        where_ (comment_tag ^. CommentTagComment ==. val comment_id)
        return tag
