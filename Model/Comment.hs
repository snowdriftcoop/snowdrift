module Model.Comment
    ( ClosureMap
    , FlagMap
    , TicketMap
    , approveComment
    , buildCommentForest
    , buildCommentTree
    , canDeleteComment
    , canEditComment
    , deleteComment
    , editComment
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
    , getCommentFlagging
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
    , makeFlagMap
    , makeModeratedComment
    , makeTicketMap
    , newClosedCommentClosure
    , newRetractedCommentClosure
    , subGetCommentAncestors
    -- SQL expressions/queries
    , exprPermissionFilter
    , exprUnapproved
    -- Probably shouldn't be exported
    , makeViewerInfo
    ) where

import Import

import           Model.User     (isProjectModerator')

import           Data.Foldable  (Foldable)
import qualified Data.Foldable  as F
import qualified Data.Map       as M
import qualified Data.Set       as S
import qualified Data.Text      as T
import           Data.Tree
import           GHC.Exts       (IsList(..))
import           Prelude        (head)
import           Yesod.Markdown (Markdown(..))

type ClosureMap = Map CommentId CommentClosure
type TicketMap  = Map CommentId (Entity Ticket)
type FlagMap    = Map CommentId (Maybe Markdown, [FlagReason])

approveComment :: UserId -> CommentId -> YesodDB App ()
approveComment user_id comment_id = liftIO getCurrentTime >>= \now ->
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

-- | Delete-cascade a comment from the database.
deleteComment :: CommentId -> YesodDB App ()
deleteComment = deleteCascade

-- | Edit a comment's text. If the comment was flagged, unflag it and send a
-- message to the flagger.
editComment :: CommentId -> Markdown -> Handler ()
editComment comment_id text = do
    runDB (updateCommentText >> getCommentFlagging comment_id) >>= \case
        Nothing -> return ()
        Just (Entity comment_flagging_id CommentFlagging{..}) -> do
            let permalink_route = DiscussCommentR
                                    commentFlaggingProjectHandle
                                    commentFlaggingTarget
                                    comment_id
            permalink_text <- getUrlRender <*> pure permalink_route
            let message_text = Markdown $ "A comment you flagged has been edited and reposted to the site. You can view it [here](" <> permalink_text <> ")."
            now <- liftIO getCurrentTime
            runDB $ do
                deleteCascade comment_flagging_id -- delete flagging and all flagging reasons with it.
                snowdrift_id <- getSnowdriftId
                insert_ $ Message
                            (Just snowdrift_id)
                            now
                            Nothing
                            (Just $ commentFlaggingFlagger)
                            message_text
                            True
  where
    updateCommentText =
        update $ \c -> do
        set c [ CommentText =. val text ]
        where_ (c ^. CommentId ==. val comment_id)

-- | Flag a comment. Send a message to the poster about the flagging. Return whether
-- or not the flag was successful (fails if the comment was already flagged.)
flagComment :: Text -> Text -> CommentId -> Text -> UserId -> [FlagReason] -> Maybe Markdown -> YesodDB App Bool
flagComment project_handle target comment_id permalink_route flagger_id reasons message = do
    poster_id <- commentUser <$> get404 comment_id
    now <- liftIO getCurrentTime
    insertUnique (CommentFlagging now flagger_id comment_id project_handle target message) >>= \case
        Nothing -> return False
        Just flagging_id -> do
            void $ insertMany (map (CommentFlaggingReason flagging_id) reasons)

            let message_text = Markdown . T.unlines $
                    [ "One of your comments has been flagged as not meeting the standards of the Code of Conduct. We *want* your involvement as long as it is respectful and friendly, so please don’t feel discouraged."
                    , ""
                    , "Please follow the link below for clarification or suggestions the flagger may have offered, and take this change to improve your tone and clarify any misunderstanding. Your comment will be made publicly visible after being edited."
                    , ""
                    , "Please alert a moderator if you believe that this flagging is inappropriate, that your posts are being excessively flagged, that the feedback from the flagger is itself a violation of the Code of Conduct, or if you want other assistance."
                    , ""
                    , "[link to flagged comment](" <> permalink_route <> ")"
                    ]
            snowdrift_id <- getSnowdriftId
            insert_ $ Message (Just snowdrift_id) now Nothing (Just poster_id) message_text True
            return True

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
getCommentAncestors = fmap (map unValue) . select . querAncestors

subGetCommentAncestors :: CommentId -> SqlExpr (ValueList CommentId)
subGetCommentAncestors = subList_select . querAncestors

getCommentDepth :: CommentId -> YesodDB App Int
getCommentDepth = fmap commentDepth . getJust

getCommentDepth404 :: CommentId -> Handler Int
getCommentDepth404 = fmap commentDepth . runDB . get404

-- | Get the "true" CommentId for this comment (i.e. take the rethread train to
-- the very last stop). Makes an unbounded number of queries.
getCommentDestination :: CommentId -> YesodDB App CommentId
getCommentDestination comment_id = getCommentRethread comment_id >>= maybe (return comment_id) getCommentDestination

-- | Get the CommentFlagging even for this Comment, if there is one.
getCommentFlagging :: CommentId -> YesodDB App (Maybe (Entity CommentFlagging))
getCommentFlagging = getBy . UniqueCommentFlagging

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
getCommentDescendantsIds = fmap (map unValue) . select . querDescendants

-- | Get all descendants of the given root comment.
getCommentDescendants :: Maybe UserId -> ProjectId -> CommentId -> YesodDB App [Entity Comment]
getCommentDescendants mviewer_id project_id root_id = makeViewerInfo mviewer_id project_id >>= \viewer_info ->
    select $
    from $ \c -> do
    where_ $
        c ^. CommentId `in_` subList_select (querDescendants root_id) &&.
        exprPermissionFilter viewer_info c
    -- DO NOT change ordering here! buildCommentTree relies on it.
    orderBy [asc (c ^. CommentParent), asc (c ^. CommentCreatedTs)]
    return c

-- | Get all descendants of all given root comments.
getCommentsDescendants :: Maybe UserId -> ProjectId -> [CommentId] -> YesodDB App [Entity Comment]
getCommentsDescendants mviewer_id project_id root_ids = makeViewerInfo mviewer_id project_id >>= \viewer_info ->
    select $
    from $ \c -> do
    where_ $
        c ^. CommentId `in_` subList_select (querAllDescendants root_ids) &&.
        exprPermissionFilter viewer_info c
    -- DO NOT change ordering here! buildCommentTree relies on it.
    orderBy [asc (c ^. CommentParent), asc (c ^. CommentCreatedTs)]
    return c

-- | Get all Comments on a Discussion that are root comments.
getAllRootComments :: Maybe UserId -> ProjectId -> DiscussionId -> YesodDB App [Entity Comment]
getAllRootComments mviewer_id project_id discussion_id = makeViewerInfo mviewer_id project_id >>= \viewer_info ->
    select $
    from $ \c -> do
    where_ $
        exprOnDiscussion discussion_id c &&.
        exprRoot c &&.
        exprPermissionFilter viewer_info c
    return c

getAllClosedRootComments :: Maybe UserId -> ProjectId -> DiscussionId -> YesodDB App [Entity Comment]
getAllClosedRootComments mviewer_id project_id discussion_id = makeViewerInfo mviewer_id project_id >>= \viewer_info ->
    select $
    from $ \c -> do
    where_ $
        exprOnDiscussion discussion_id c &&.
        exprRoot c &&.
        exprClosed c &&.
        exprPermissionFilter viewer_info c
    return c

getAllOpenRootComments :: Maybe UserId -> ProjectId -> DiscussionId -> YesodDB App [Entity Comment]
getAllOpenRootComments mviewer_id project_id discussion_id = makeViewerInfo mviewer_id project_id >>= \viewer_info ->
    select $
    from $ \c -> do
    where_ $
        exprOnDiscussion discussion_id c &&.
        exprRoot c &&.
        exprOpen c &&.
        exprPermissionFilter viewer_info c
    return c

-- | Get a Comment's Tags.
getTags :: CommentId -> YesodDB App [Entity Tag]
getTags comment_id =
    select $
    from $ \(ct `InnerJoin` t) -> do
    on_ (ct ^. CommentTagTag ==. t ^. TagId)
    where_ (ct ^. CommentTagComment ==. val comment_id)
    return t

makeClosureMap :: (IsList c, CommentId ~ Item c) => c -> YesodDB App ClosureMap
makeClosureMap comment_ids = fmap (M.fromList . map ((commentClosureComment &&& id) . entityVal)) $
    select $
    from $ \c -> do
    where_ (c ^. CommentClosureComment `in_` valList comment_ids)
    return c

-- Given a collection of CommentId, make a map from CommentId to Entity Ticket. Comments that
-- are not tickets will simply not be in the map.
makeTicketMap :: (IsList c, CommentId ~ Item c) => c -> YesodDB App TicketMap
makeTicketMap comment_ids = fmap (M.fromList . map ((ticketComment . entityVal) &&& id)) $
    select $
    from $ \t -> do
    where_ (t ^. TicketComment `in_` valList comment_ids)
    return t

makeFlagMap :: (IsList c, CommentId ~ Item c) => c -> YesodDB App FlagMap
makeFlagMap comment_ids = mkFlagMap <$> getCommentFlaggings
  where
    getCommentFlaggings :: YesodDB App [(CommentId, Maybe Markdown, FlagReason)]
    getCommentFlaggings = fmap (map unwrapValues) $
        select $
        from $ \(cf `InnerJoin` cfr) -> do
        on_ (cf ^. CommentFlaggingId ==. cfr ^. CommentFlaggingReasonFlagging)
        where_ (cf ^. CommentFlaggingComment `in_` valList comment_ids)
        return (cf ^. CommentFlaggingComment, cf ^. CommentFlaggingMessage, cfr ^. CommentFlaggingReasonReason)

    mkFlagMap :: [(CommentId, Maybe Markdown, FlagReason)] -> FlagMap
    mkFlagMap = foldr (\(comment_id, message, reason) -> M.insertWith combine comment_id (message, [reason])) mempty
      where
        combine :: (Maybe Markdown, [FlagReason]) -> (Maybe Markdown, [FlagReason]) -> (Maybe Markdown, [FlagReason])
        combine (message, reasons1) (_, reasons2) = (message, reasons1 <> reasons2)

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


-- | Dumb helper function to make a "viewer info" argument for exprPermissionFilter.
-- Unfortunately we export it as another module uses exprPermissionFilter. Probably
-- this should be rectified.
makeViewerInfo :: Maybe UserId -> ProjectId -> YesodDB App (Maybe (UserId, Bool))
makeViewerInfo Nothing _ = return Nothing
makeViewerInfo (Just viewer_id) project_id = Just . (viewer_id,) <$> isProjectModerator' viewer_id project_id

--------------------------------------------------------------------------------

exprClosed, exprOpen :: SqlExpr (Entity Comment) -> SqlExpr (Value Bool)
exprClosed c = c ^. CommentId `in_`   exprClosedCommentIds
exprOpen   c = c ^. CommentId `notIn` exprClosedCommentIds

exprClosedCommentIds :: SqlExpr (ValueList CommentId)
exprClosedCommentIds =
    subList_select $
    from $ \cl ->
    return (cl ^. CommentClosureComment)

-- | Comment is root?
exprRoot :: SqlExpr (Entity Comment) -> SqlExpr (Value Bool)
exprRoot c = isNothing (c ^. CommentParent)

-- | Comment on this Discussion?
exprOnDiscussion :: DiscussionId -> SqlExpr (Entity Comment) -> SqlExpr (Value Bool)
exprOnDiscussion discussion_id c = c ^. CommentDiscussion ==. val discussion_id

-- | SQL expression to filter a comment based on "permissions", as follows:
--    If moderator, show all.
--    If logged in, show all approved (hiding flagged), plus own comments (unapproved + flagged).
--    If not logged in, show all approved (hiding flagged).
--    No matter what, hide rethreaded comments (they've essentially been replaced).
exprPermissionFilter :: Maybe (UserId, Bool) -> SqlExpr (Entity Comment) -> SqlExpr (Value Bool)
exprPermissionFilter (Just (_,True))      c = exprNotRethreaded c
exprPermissionFilter (Just (viewer_id,_)) c = exprNotRethreaded c &&. (exprApprovedAndNotFlagged c ||. exprPostedBy viewer_id c)
exprPermissionFilter Nothing              c = exprNotRethreaded c &&. exprApprovedAndNotFlagged c
    -- is_mod <- isProjectModerator' viewer_id project_id

exprNotRethreaded :: SqlExpr (Entity Comment) -> SqlExpr (Value Bool)
exprNotRethreaded c = c ^. CommentId `notIn` rethreadedCommentIds
  where
    rethreadedCommentIds :: SqlExpr (ValueList CommentId)
    rethreadedCommentIds =
        subList_select $
        from $ \r ->
        return (r ^. RethreadOldComment)

exprApproved :: SqlExpr (Entity Comment) -> SqlExpr (Value Bool)
exprApproved = not_ . exprUnapproved

exprUnapproved :: SqlExpr (Entity Comment) -> SqlExpr (Value Bool)
exprUnapproved c = isNothing (c ^. CommentModeratedTs)

exprNotFlagged :: SqlExpr (Entity Comment) -> SqlExpr (Value Bool)
exprNotFlagged c = c ^. CommentId `notIn` flaggedCommentIds
  where
    flaggedCommentIds :: SqlExpr (ValueList CommentId)
    flaggedCommentIds =
        subList_select $
        from $ \cf ->
        return (cf ^. CommentFlaggingComment)

exprApprovedAndNotFlagged :: SqlExpr (Entity Comment) -> SqlExpr (Value Bool)
exprApprovedAndNotFlagged c = exprApproved c &&. exprNotFlagged c

exprPostedBy :: UserId -> SqlExpr (Entity Comment) -> SqlExpr (Value Bool)
exprPostedBy user_id c = c ^. CommentUser ==. val user_id

querAncestors :: CommentId -> SqlQuery (SqlExpr (Value CommentId))
querAncestors comment_id =
    from $ \ca -> do
    where_ (ca ^. CommentAncestorComment ==. val comment_id)
    return (ca ^. CommentAncestorAncestor)

querDescendants :: CommentId -> SqlQuery (SqlExpr (Value CommentId))
querDescendants comment_id =
    from $ \ca -> do
    where_ (ca ^. CommentAncestorAncestor ==. val comment_id)
    return (ca ^. CommentAncestorComment)

querAllDescendants :: [CommentId] -> SqlQuery (SqlExpr (Value CommentId))
querAllDescendants comment_ids =
    from $ \ca -> do
    where_ (ca ^. CommentAncestorAncestor `in_` valList comment_ids)
    return (ca ^. CommentAncestorComment)
