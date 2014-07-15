module Model.Comment
    ( ClosureMap
    , TicketMap
    , approveComment
    , buildCommentForest
    , buildCommentTree
    , canDeleteComment
    , canEditComment
    , deleteComment
    , editComment
    , filterComments
    , flagComment
    , getAllClosedRootComments
    , getAllOpenRootComments
    , getAllRootComments
    , getAncestorClosures
    , getAncestorClosures'
    , getCommentAncestors
    , getCommentDepth
    , getCommentDepth404
    , getCommentDescendants
    , getCommentDescendantsIds
    , getCommentsDescendants
    , getCommentDestination
    , getCommentPage
    , getCommentPageId
    , getCommentRethread
    , getCommentTags
    , getCommentsUsers
    , getTags
    , isApproved
    , isEvenDepth
    , isOddDepth
    , isTopLevel
    , makeClosureMap
    , makeModeratedComment
    , makeTicketMap
    , newClosedCommentClosure
    , newRetractedCommentClosure
    , subGetCommentAncestors
    ) where

import Import

import           Model.User    (isProjectModerator')

import           Data.Foldable (Foldable)
import qualified Data.Foldable as F
import qualified Data.Map      as M
import qualified Data.Set      as S
import           Data.Tree
import           GHC.Exts      (IsList(..))
import           Prelude       (head)

type ClosureMap = Map CommentId CommentClosure
type TicketMap  = Map CommentId (Entity Ticket)

approveComment :: UserId -> CommentId -> YesodDB App ()
approveComment user_id comment_id = do
    now <- liftIO getCurrentTime
    update $ \c -> do
        set c [ CommentModeratedTs =. val (Just now)
              , CommentModeratedBy =. val (Just user_id)
              ]
        where_ (c ^. CommentId ==. val comment_id)

isApproved :: Comment -> Bool
isApproved = isJust . commentModeratedTs

isTopLevel :: Comment -> Bool
isTopLevel = (== 0) . commentDepth

isEvenDepth :: Comment -> Bool
isEvenDepth comment = not (isTopLevel comment) && commentDepth comment `mod` 2 == 1

isOddDepth :: Comment -> Bool
isOddDepth comment = not (isTopLevel comment) && not (isEvenDepth comment)

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

canDeleteComment :: UserId -> Entity Comment -> YesodDB App Bool
canDeleteComment user_id (Entity comment_id comment) = do
    if commentUser comment /= user_id
        then return False
        else do
          descendants_ids <- getCommentDescendantsIds comment_id
          if null descendants_ids
              then return True
              else return False

canEditComment :: UserId -> Comment -> Bool
canEditComment user_id = (user_id ==) . commentUser

-- | Delete a comment from the database. Should not be called on comments with
-- children, as their foreign keys would then cause runtime errors.
deleteComment :: CommentId -> YesodDB App ()
deleteComment = deleteKey

-- | Edit a comment's text.
editComment :: CommentId -> Markdown -> YesodDB App ()
editComment comment_id text =
    update $ \c -> do
    set c [ CommentText =. val text ]
    where_ (c ^. CommentId ==. val comment_id)

-- | Flag a comment.
flagComment :: CommentId -> UserId -> [FlagReason] -> Maybe Markdown -> YesodDB App ()
flagComment comment_id user_id reasons message = do
    now <- liftIO getCurrentTime
    flagging_id <- insert (CommentFlagging now user_id comment_id message)
    void $ insertMany (map (CommentFlaggingReason flagging_id) reasons)

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
            from $ \cc -> do
            where_ (cc ^. CommentClosureComment `in_` valList all_comment_ids)
            return cc

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

getCommentDepth404 :: CommentId -> Handler Int
getCommentDepth404 = fmap commentDepth . runDB . get404

-- | Get the "true" CommentId for this comment (i.e. take the rethread train to
-- the very last stop). Makes an unbounded number of queries.
getCommentDestination :: CommentId -> YesodDB App CommentId
getCommentDestination comment_id = getCommentRethread comment_id >>= maybe (return comment_id) getCommentDestination

-- | Partial function.
getCommentPage :: CommentId -> YesodDB App WikiPage
getCommentPage = fmap entityVal . getCommentPageEntity

-- | Partial function.
getCommentPageId :: CommentId -> YesodDB App WikiPageId
getCommentPageId = fmap entityKey . getCommentPageEntity

-- | Partial function. Fails if the given Comment is not on a WikiPage, but some
-- other Discussion.
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

-- | Get a Comment's descendants' ids (don't filter hidden or unmoderated comments).
getCommentDescendantsIds :: CommentId -> YesodDB App [CommentId]
getCommentDescendantsIds comment_id = fmap (map unValue) $
    select $
        from $ \ca -> do
        where_ (ca ^. CommentAncestorAncestor ==. val comment_id)
        return (ca ^. CommentAncestorComment)

-- | Get all descendants of the given root comment.
-- TODO(mitchell): Share code with with getCommentsDescendants
getCommentDescendants :: Maybe UserId -> ProjectId -> CommentId -> YesodDB App [Entity Comment]
getCommentDescendants mviewer_id project_id root_id = filterComments mviewer_id project_id <*> go
  where
    go =
        select $
            from $ \c -> do
            where_ (c ^. CommentId `in_`
                (subList_select $
                    from $ \ca -> do
                    where_ (ca ^. CommentAncestorAncestor ==. val root_id)
                    return (ca ^. CommentAncestorComment)))
            -- DO NOT change ordering here! buildCommentTree relies on it.
            orderBy [asc (c ^. CommentParent), asc (c ^. CommentCreatedTs)]
            return c

-- | Get all descendants of all given root comments.
getCommentsDescendants :: Maybe UserId -> ProjectId -> [CommentId] -> YesodDB App [Entity Comment]
getCommentsDescendants mviewer_id project_id root_ids = filterComments mviewer_id project_id <*> go
  where
    go =
        select $
            from $ \c -> do
            where_ (c ^. CommentId `in_`
                (subList_select $
                    from $ \ca -> do
                    where_ (ca ^. CommentAncestorAncestor `in_` valList root_ids)
                    return (ca ^. CommentAncestorComment)))
            -- DO NOT change ordering here! buildCommentTree relies on it.
            orderBy [asc (c ^. CommentParent), asc (c ^. CommentCreatedTs)]
            return c

-- | Get all Comments on a Discussion that are root comments.
getAllRootComments :: Maybe UserId -> ProjectId -> DiscussionId -> YesodDB App [Entity Comment]
getAllRootComments mviewer_id project_id discussion_id = filterComments mviewer_id project_id <*> go
  where
    go =
        select $
            from $ \c -> do
            where_ $
                exprCommentOnDiscussion discussion_id c &&.
                exprCommentIsRoot c
            return c

getAllClosedRootComments :: Maybe UserId -> ProjectId -> DiscussionId -> YesodDB App [Entity Comment]
getAllClosedRootComments mviewer_id project_id discussion_id = filterComments mviewer_id project_id <*> go
  where
    go =
        select $
            from $ \c -> do
            where_ $
                exprCommentOnDiscussion discussion_id c &&.
                exprCommentIsRoot c &&.
                exprCommentIsClosed c
            return c

getAllOpenRootComments :: Maybe UserId -> ProjectId -> DiscussionId -> YesodDB App [Entity Comment]
getAllOpenRootComments mviewer_id project_id discussion_id = filterComments mviewer_id project_id <*> go
  where
    go =
      select $
          from $ \c -> do
          where_ $
              exprCommentOnDiscussion discussion_id c &&.
              exprCommentIsRoot c &&.
              exprCommentIsOpen c
          return c

-- | Filter comments per the following rules:
-- Hidden comments (rethreaded or flagged) are not shown.
-- Otherwise, if the viewer is...
--    a moderator: show all
--    logged in: show all moderated, plus unmoderated 'own' comments
--    not logged in: show all moderated
filterComments :: Maybe UserId -> ProjectId -> YesodDB App ([Entity Comment] -> [Entity Comment])
filterComments mviewer_id project_id = do
    filter_one <- (\hidden_ids -> flip S.notMember hidden_ids . entityKey) <$> getHiddenCommentIds
    filter_two <- makeCommentFilter
    return $ filter (\c -> filter_one c && filter_two c)
  where
    makeCommentFilter :: YesodDB App (Entity Comment -> Bool)
    makeCommentFilter = case mviewer_id of
        Nothing -> return isModerated
        Just viewer_id -> do
            is_moderator <- isProjectModerator' viewer_id project_id
            if is_moderator
                then return $ const True
                else return $ \c -> isModerated c || isOwnComment viewer_id c
      where
        isModerated :: Entity Comment -> Bool
        isModerated = isJust . commentModeratedTs . entityVal

        isOwnComment :: UserId -> Entity Comment -> Bool
        isOwnComment user_id (Entity _ Comment{..}) = user_id == commentUser

    getHiddenCommentIds :: YesodDB App (Set CommentId)
    getHiddenCommentIds = (<>) <$> rethreadedCommentIds <*> flaggedCommentIds
      where
        rethreadedCommentIds :: YesodDB App (Set CommentId)
        rethreadedCommentIds = fmap (S.fromList . map unValue) $
            select $
                from $ \r ->
                return (r ^. RethreadOldComment)

        flaggedCommentIds :: YesodDB App (Set CommentId)
        flaggedCommentIds = fmap (S.fromList . map unValue) $
            select $
                from $ \f ->
                return (f ^. CommentFlaggingComment)

-- | Comment is closed?
exprCommentIsClosed :: SqlExpr (Entity Comment) -> SqlExpr (Value Bool)
exprCommentIsClosed c = c ^. CommentId `in_`   subList_select (from $ \cl -> return (cl ^. CommentClosureComment))

-- | Comment is open?
exprCommentIsOpen :: SqlExpr (Entity Comment) -> SqlExpr (Value Bool)
exprCommentIsOpen c = c ^. CommentId `notIn` subList_select (from $ \cl -> return (cl ^. CommentClosureComment))

-- | Comment is root?
exprCommentIsRoot :: SqlExpr (Entity Comment) -> SqlExpr (Value Bool)
exprCommentIsRoot c = isNothing (c ^. CommentParent)

-- | Comment on this Discussion?
exprCommentOnDiscussion :: DiscussionId -> SqlExpr (Entity Comment) -> SqlExpr (Value Bool)
exprCommentOnDiscussion discussion_id c = c ^. CommentDiscussion ==. val discussion_id

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

newClosedCommentClosure, newRetractedCommentClosure :: MonadIO m => UserId -> Markdown -> CommentId -> m CommentClosure
newClosedCommentClosure    = newCommentClosure Closed
newRetractedCommentClosure = newCommentClosure Retracted

newCommentClosure :: MonadIO m => ClosureType -> UserId -> Markdown -> CommentId -> m CommentClosure
newCommentClosure closure_type user_id reason comment_id =
    (\now -> CommentClosure now user_id closure_type reason comment_id) `liftM` liftIO getCurrentTime

-- | Construct a comment, auto-moderated by 'this' User (because they are established).
makeModeratedComment :: MonadIO m => UserId -> DiscussionId -> Maybe CommentId -> Markdown -> Int -> m Comment
makeModeratedComment user_id discussion_id parent_comment comment_text depth = do
    now <- liftIO getCurrentTime
    return $ Comment
                 now
                 (Just now)
                 (Just user_id)
                 discussion_id
                 parent_comment
                 user_id
                 comment_text
                 depth

-- | Get the set of Users that have posted the given Foldable of comments.
getCommentsUsers :: Foldable f => f (Entity Comment) -> Set UserId
getCommentsUsers = F.foldMap (S.singleton . commentUser . entityVal)
