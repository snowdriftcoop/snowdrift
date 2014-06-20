module Model.Comment
    ( buildCommentForest
    , buildCommentTree
    , getAncestorClosures
    , getAncestorClosures'
    , getCommentAncestors
    , getCommentDescendants
    , getCommentDestination
    , getCommentDepth
    , getCommentPage
    , getCommentPageId
    , getCommentRethread
    , getCommentTags
    , getRepliesComments
    , getRootComments
    , getTags
    , makeClosureMap
    , makeTicketMap
    , newCommentClosure
    , subGetCommentAncestors
    ) where

import Import

import qualified Data.Map          as M
import           Data.Tree
import           GHC.Exts          (IsList(..))
import           Prelude           (head)

import           Model.ClosureType (ClosureType)

-- | Build a tree of comments, given the root and replies. The replies are not necessarily
-- direct or indirect descendants of the root, but rather may be siblings, nephews, etc.
-- This is done to greatly simplify the calling code of this function.
--
-- THIS FUNCTION RELIES ON THE SORT ORDER OF THE REPLIES! Specifically, they must be sorted
-- in ascending-parent-id major, ascending-timestamp minor order.
buildCommentTree :: (Entity Comment, [Entity Comment]) -> Tree (Entity Comment)
buildCommentTree = unfoldTree step
  where
    step :: (Entity Comment, [Entity Comment]) -> (Entity Comment, [(Entity Comment, [Entity Comment])])
    step (root, replies) = (root, children_and_their_descendants)
      where
        descendants :: [Entity Comment]
        descendants = dropWhile (not . isParentOf root) replies

        -- children :: [Entity Comment]
        -- children_descendants :: [Entity Comment]
        (children, children_descendants) = span (isParentOf root) descendants

        children_and_their_descendants :: [(Entity Comment, [Entity Comment])]
        children_and_their_descendants = map (, children_descendants) children

        isParentOf :: Entity Comment -> Entity Comment -> Bool
        isParentOf (Entity parent_key _) (Entity _ child) = Just parent_key == commentParent child

buildCommentForest :: [Entity Comment]                                             -- root comments
                   -> [Entity Comment]                                             -- replies comments
                   -> Forest (Entity Comment)
buildCommentForest roots replies = (map (buildCommentTree . (, replies))) roots

-- | Get all ancestors that have been closed.
getAncestorClosures :: CommentId -> YesodDB App [CommentClosure]
getAncestorClosures comment_id = fmap (map entityVal) $
    select $
        from $ \(ca `InnerJoin` cc) -> do
        on_ (ca ^. CommentAncestorAncestor ==. cc ^. CommentClosureComment)
        orderBy [asc (cc ^. CommentClosureComment)]
        where_ (ca ^. CommentAncestorComment ==. val comment_id)
        return cc

-- | Get all ancestors, including this comment, that have been closed.
getAncestorClosures' :: CommentId -> YesodDB App [CommentClosure]
getAncestorClosures' comment_id = do
    all_comment_ids <- (comment_id :) <$> getCommentAncestors comment_id
    fmap (map entityVal) $
        select $
            from $ \c -> do
            where_ (c ^. CommentClosureComment `in_` valList all_comment_ids)
            return c

-- | Get a comment's ancestors' ids.
getCommentAncestors :: CommentId -> YesodDB App [CommentId]
getCommentAncestors = fmap (map unValue) . select . ancestorsQuery

subGetCommentAncestors :: CommentId -> SqlExpr (ValueList CommentId)
subGetCommentAncestors = subList_select . ancestorsQuery

ancestorsQuery :: CommentId -> SqlQuery (SqlExpr (Value CommentId))
ancestorsQuery comment_id =
    from $ \ca -> do
    where_ (ca ^. CommentAncestorComment ==. val comment_id)
    return (ca ^. CommentAncestorAncestor)

getCommentDepth :: CommentId -> YesodDB App Int
getCommentDepth = fmap commentDepth . getJust

-- | Get a comment's descendants' ids.
getCommentDescendants :: CommentId -> YesodDB App [CommentId]
getCommentDescendants comment_id = fmap (map unValue) $
    select $
        from $ \ca -> do
        where_ (ca ^. CommentAncestorAncestor ==. val comment_id)
        return (ca ^. CommentAncestorComment)

-- | Get the "true" CommentId for this comment (i.e. take the rethread train to
-- the very last stop). Makes an unbounded number of queries.
getCommentDestination :: CommentId -> YesodDB App CommentId
getCommentDestination comment_id = getCommentRethread comment_id >>= maybe (return comment_id) getCommentDestination

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

-- | Get the CommentId this CommentId was rethreaded to, if it was.
getCommentRethread :: CommentId -> YesodDB App (Maybe CommentId)
getCommentRethread comment_id = fmap unValue . listToMaybe <$> (
    select $
        from $ \cr -> do
        where_ $ cr ^. CommentRethreadOldComment ==. val comment_id
        return $ cr ^. CommentRethreadNewComment)

-- | Get a Comment's CommentTags.
getCommentTags :: CommentId -> YesodDB App [Entity CommentTag]
getCommentTags comment_id =
    select $
        from $ \comment_tag -> do
        where_ $ comment_tag ^. CommentTagComment ==. val comment_id
        return comment_tag

-- | Get all Comments on a Discussion that aren't root comments.
getRepliesComments :: Bool -> DiscussionId -> YesodDB App [Entity Comment]
getRepliesComments is_moderator discussion_id =
    select $
        from $ \comment -> do
        where_ $
            genericCommentConditions is_moderator discussion_id comment &&.
            not_ (isNothing $ comment ^. CommentParent)
        -- DO NOT change ordering here! buildCommentTree relies on it.
        orderBy [asc (comment ^. CommentParent), asc (comment ^. CommentCreatedTs)]
        return comment

-- | Get all Comments on a Discussion that are root comments.
getRootComments :: Bool -> DiscussionId -> YesodDB App [Entity Comment]
getRootComments is_moderator discussion_id =
    select $
        from $ \comment -> do
        where_ $
            genericCommentConditions is_moderator discussion_id comment &&.
            isNothing (comment ^. CommentParent)
        return comment

-- | Generic comment conditions that apply to both root comments and child comments.
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

-- | Get a Comment's Tags.
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

newCommentClosure :: MonadIO m => UserId -> ClosureType -> Markdown -> CommentId -> m CommentClosure
newCommentClosure user_id closure_type reason comment_id =
    (\now -> CommentClosure now user_id closure_type reason comment_id) `liftM` liftIO getCurrentTime
