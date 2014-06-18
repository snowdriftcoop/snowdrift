module Model.Comment
    ( getAncestorClosures
    , getCommentAncestors
    , getCommentPage
    , getCommentPageId
    , getCommentRethread
    , getCommentTags
    , getRepliesComments
    , getRootComments
    , getTags
    , makeClosureMap
    , makeTicketMap
    ) where

import Import

import qualified Data.Map   as M
import           Data.Maybe (listToMaybe)
import           GHC.Exts   (IsList(..))
import           Prelude    (head)

-- Get all ancestors that have been closed.
getAncestorClosures :: CommentId -> YesodDB App [CommentClosure]
getAncestorClosures comment_id = fmap (map entityVal) $
    select $
        from $ \(comment_ancestor `InnerJoin` closure) -> do
        on_ (comment_ancestor ^. CommentAncestorAncestor ==. closure ^. CommentClosureComment)
        where_ (comment_ancestor ^. CommentAncestorComment ==. val comment_id)
        return closure

-- Get a comment's ancestors' ids.
getCommentAncestors :: CommentId -> YesodDB App [CommentId]
getCommentAncestors comment_id = fmap (map unValue) $
    select $
        from $ \ca -> do
        where_ (ca ^. CommentAncestorAncestor ==. val comment_id)
        return (ca^.CommentAncestorComment)

getCommentPage :: CommentId -> YesodDB App WikiPage
getCommentPage = fmap entityVal . getCommentPageEntity

getCommentPageId :: CommentId -> YesodDB App WikiPageId
getCommentPageId = fmap entityKey . getCommentPageEntity

getCommentPageEntity :: CommentId -> YesodDB App (Entity WikiPage)
getCommentPageEntity comment_id = fmap head $
    select $
        from $ \(c `InnerJoin` p) -> do
        on_ (c ^. CommentDiscussion ==. p ^. WikiPageDiscussion)
        where_ (c ^. CommentId ==. val comment_id)
        return p

-- Get the CommentId this CommentId was rethreaded to, if it was.
getCommentRethread :: CommentId -> YesodDB App (Maybe CommentId)
getCommentRethread comment_id = fmap unValue . listToMaybe <$> (
    select $
        from $ \comment_rethread -> do
        where_ $ comment_rethread ^. CommentRethreadOldComment ==. val comment_id
        return $ comment_rethread ^. CommentRethreadNewComment)

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

makeClosureMap :: (IsList c, CommentId ~ Item c) => c -> YesodDB App (Map CommentId CommentClosure)
makeClosureMap comment_ids = fmap (M.fromList . map ((commentClosureComment &&& id) . entityVal)) $
    select $
        from $ \ closure -> do
        where_ (closure ^. CommentClosureComment `in_` valList comment_ids)
        return closure

-- Given a collection of CommentId, make a map from CommentId to Entity Ticket. Comments that
-- are not tickets will simply not be in the map.
makeTicketMap :: (IsList c, CommentId ~ Item c) => c -> YesodDB App (Map CommentId (Entity Ticket))
makeTicketMap comment_ids = fmap (M.fromList . map ((ticketComment . entityVal) &&& id)) $
    select $
        from $ \t -> do
        where_ (t ^. TicketComment `in_` valList comment_ids)
        return t
