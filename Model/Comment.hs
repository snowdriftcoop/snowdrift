module Model.Comment
    -- Types
    ( ClosureMap
    , FlagMap
    , TicketMap
    -- Utility functions
    , buildCommentForest
    , buildCommentTree
    , commentIsApproved
    , commentIsEvenDepth
    , commentIsFlagged
    , commentIsOddDepth
    , commentIsTopLevel
    , makeCommentUsersSet
    , makeModeratedComment
    , newClosedCommentClosure
    , newRetractedCommentClosure
    -- Database actions
    , approveCommentDB
    , deleteCommentDB
    , editCommentDB
    , flagCommentDB
    , fetchAllClosedRootCommentsDB
    , fetchAllOpenRootCommentsDB
    , fetchAllRootCommentsDB
    , fetchAncestorClosuresDB
    , fetchAncestorClosuresDB'
    , fetchCommentAncestorsDB
    , fetchCommentCommentTagsDB
    , fetchCommentDepthDB
    , fetchCommentDepth404DB
    , fetchCommentDescendantsDB
    , fetchCommentDescendantsIdsDB
    , fetchCommentDestinationDB
    , fetchCommentFlaggingDB
    , fetchCommentsDescendantsDB
    , fetchCommentPageEntityDB
    , fetchCommentRethreadDB
    , fetchCommentTagsDB
    , filterCommentsDB
    , insertApprovedCommentDB
    , insertUnapprovedCommentDB
    , makeClosureMapDB
    , makeFlagMapDB
    , makeTicketMapDB
    , rethreadCommentsDB
    , subFetchCommentAncestorsDB
    , unsafeFetchCommentPageDB
    , unsafeFetchCommentPageIdDB
    ) where

import Import

import           Model.Comment.Sql
import           Model.Message

import qualified Control.Monad.State         as St
import           Control.Monad.Writer.Strict (tell)
import           Data.Foldable               (Foldable)
import qualified Data.Foldable               as F
import qualified Data.Map                    as M
import           Data.Maybe                  (fromJust)
import qualified Data.Set                    as S
import qualified Data.Text                   as T
import           Data.Tree
import           GHC.Exts                    (IsList(..))
import           Yesod.Markdown              (Markdown(..))

--------------------------------------------------------------------------------
-- Types

type ClosureMap = Map CommentId CommentClosure
type TicketMap  = Map CommentId (Entity Ticket)
type FlagMap    = Map CommentId (Maybe Markdown, [FlagReason])

--------------------------------------------------------------------------------
-- Utility functions

commentIsApproved :: Comment -> Bool
commentIsApproved = isJust . commentModeratedTs

commentIsTopLevel :: Comment -> Bool
commentIsTopLevel = (== 0) . commentDepth

commentIsEvenDepth :: Comment -> Bool
commentIsEvenDepth comment = not (commentIsTopLevel comment) && commentDepth comment `mod` 2 == 1

commentIsOddDepth :: Comment -> Bool
commentIsOddDepth comment = not (commentIsTopLevel comment) && not (commentIsEvenDepth comment)

commentIsFlagged :: CommentId -> DB Bool
commentIsFlagged = fmap (maybe False (const True)) . getBy . UniqueCommentFlagging

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
makeCommentUsersSet :: Foldable f => f (Entity Comment) -> Set UserId
makeCommentUsersSet = F.foldMap (S.singleton . commentUser . entityVal)

--------------------------------------------------------------------------------
-- Database actions

approveCommentDB :: UserId -> CommentId -> Comment -> SDB ()
approveCommentDB user_id comment_id comment = do
    lift upd
    tell [ECommentPosted comment_id comment]
  where
    upd = liftIO getCurrentTime >>= \now ->
        update $ \c -> do
        set c [ CommentModeratedTs =. val (Just now)
              , CommentModeratedBy =. val (Just user_id)
              ]
        where_ (c ^. CommentId ==. val comment_id)

insertApprovedCommentDB :: UTCTime
                        -> UTCTime
                        -> UserId
                        -> DiscussionId
                        -> Maybe CommentId
                        -> UserId
                        -> Markdown
                        -> Int
                        -> SDB CommentId
insertApprovedCommentDB created_ts moderated_ts moderated_by discussion_id mparent_id user_id text depth =
    insertCommentDB
      (Just moderated_ts)
      (Just moderated_by)
      ECommentPosted
      created_ts
      discussion_id
      mparent_id
      user_id
      text
      depth

insertUnapprovedCommentDB :: UTCTime
                          -> DiscussionId
                          -> Maybe CommentId
                          -> UserId
                          -> Markdown
                          -> Int
                          -> SDB CommentId
insertUnapprovedCommentDB = insertCommentDB Nothing Nothing ECommentPending

insertCommentDB :: Maybe UTCTime
                -> Maybe UserId
                -> (CommentId -> Comment -> SnowdriftEvent)
                -> UTCTime
                -> DiscussionId
                -> Maybe CommentId
                -> UserId
                -> Markdown
                -> Int
                -> SDB CommentId
insertCommentDB mmoderated_ts mmoderated_by mk_event created_ts discussion_id mparent_id user_id text depth = do
    let comment = Comment
                    created_ts
                    mmoderated_ts
                    mmoderated_by
                    discussion_id
                    mparent_id
                    user_id
                    text
                    depth
    comment_id <- lift $ insert comment
    tell [mk_event comment_id comment]
    return comment_id

-- | Delete-cascade a comment from the database.
deleteCommentDB :: CommentId -> DB ()
deleteCommentDB = deleteCascade

-- | Edit a comment's text. If the comment was flagged, unflag it and send a
-- message to the flagger.
editCommentDB :: CommentId -> Markdown -> SYDB ()
editCommentDB comment_id text = do
    lift updateCommentText
    lift (fetchCommentFlaggingDB comment_id) >>= \case
        Nothing -> return ()
        Just (Entity comment_flagging_id CommentFlagging{..}) -> do
            let permalink_route = DiscussCommentR
                                    commentFlaggingProjectHandle
                                    commentFlaggingTarget
                                    comment_id
            permalink_text <- lift $ getUrlRender <*> pure permalink_route
            let message_text = Markdown $ "A comment you flagged has been edited and reposted to the site. You can view it [here](" <> permalink_text <> ")."
            lift $ deleteCascade comment_flagging_id -- delete flagging and all flagging reasons with it.
            void $ sendNotificationMessageDB MessageDirect commentFlaggingFlagger message_text
  where
    updateCommentText =
        update $ \c -> do
        set c [ CommentText =. val text ]
        where_ (c ^. CommentId ==. val comment_id)

-- | Flag a comment. Send a message to the poster about the flagging. Return whether
-- or not the flag was successful (fails if the comment was already flagged.)
flagCommentDB :: Text -> Text -> CommentId -> Text -> UserId -> [FlagReason] -> Maybe Markdown -> SYDB Bool
flagCommentDB project_handle target comment_id permalink_route flagger_id reasons message = do
    poster_id <- lift $ commentUser <$> get404 comment_id
    now <- liftIO getCurrentTime
    lift (insertUnique (CommentFlagging now flagger_id comment_id project_handle target message)) >>= \case
        Nothing -> return False
        Just flagging_id -> do
            lift $ void $ insertMany (map (CommentFlaggingReason flagging_id) reasons)

            let message_text = Markdown . T.unlines $
                    [ "Another user flagged your comment as not meeting the standards of the Code of Conduct. We *want* your involvement as long as it remains respectful and friendly, so please don’t feel discouraged."
                    , ""
                    , "Please follow the link below for clarification and suggestions the flagger may have offered, and take this chance to improve your tone and clarify any misunderstanding. Your newly edited comment will then be publicly visible again."
                    , ""
                    , "Please alert a moderator if you believe that this flagging is inappropriate, if the flagger violated the Code of Conduct in their feedback, or if you want other assistance."
                    , ""
                    , "[link to flagged comment](" <> permalink_route <> ")"
                    ]
            void $ sendNotificationMessageDB MessageDirect poster_id message_text
            return True

-- | Filter a list of comments per the permission filter (see Model.Comment.Sql.exprPermissionFilter)
filterCommentsDB :: [CommentId] -> Maybe UserId -> ProjectId -> DB [CommentId]
filterCommentsDB comment_ids muser_id project_id = fmap (map unValue) $
    select $
    from $ \c -> do
    where_ $
        c ^. CommentId `in_` valList comment_ids &&.
        exprPermissionFilter muser_id (val project_id) c
    return (c ^. CommentId)

-- | Get all ancestors that have been closed.
fetchAncestorClosuresDB :: CommentId -> DB [CommentClosure]
fetchAncestorClosuresDB comment_id = fmap (map entityVal) $
    select $
    from $ \(ca `InnerJoin` cc) -> do
    on_ (ca ^. CommentAncestorAncestor ==. cc ^. CommentClosureComment)
    orderBy [asc (cc ^. CommentClosureComment)]
    where_ (ca ^. CommentAncestorComment ==. val comment_id)
    return cc

-- | Get all ancestors, including this comment, that have been closed.
fetchAncestorClosuresDB' :: CommentId -> DB [CommentClosure]
fetchAncestorClosuresDB' comment_id = do
    all_comment_ids <- (comment_id :) <$> fetchCommentAncestorsDB comment_id
    fmap (map entityVal) $
        select $
        from $ \cc -> do
        where_ (cc ^. CommentClosureComment `in_` valList all_comment_ids)
        return cc

-- | Get a comment's ancestors' ids.
fetchCommentAncestorsDB :: CommentId -> DB [CommentId]
fetchCommentAncestorsDB = fmap (map unValue) . select . querAncestors

subFetchCommentAncestorsDB :: CommentId -> SqlExpr (ValueList CommentId)
subFetchCommentAncestorsDB = subList_select . querAncestors

fetchCommentDepthDB :: CommentId -> DB Int
fetchCommentDepthDB = fmap commentDepth . getJust

fetchCommentDepth404DB :: CommentId -> Handler Int
fetchCommentDepth404DB = fmap commentDepth . runYDB . get404

-- | Get the CommentFlagging even for this Comment, if there is one.
fetchCommentFlaggingDB :: CommentId -> DB (Maybe (Entity CommentFlagging))
fetchCommentFlaggingDB = getBy . UniqueCommentFlagging

unsafeFetchCommentPageDB :: CommentId -> DB WikiPage
unsafeFetchCommentPageDB = fmap entityVal . unsafeFetchCommentPageEntityDB

unsafeFetchCommentPageIdDB :: CommentId -> DB WikiPageId
unsafeFetchCommentPageIdDB = fmap entityKey . unsafeFetchCommentPageEntityDB

-- | Fails if the given Comment is not on a WikiPage, but some other Discussion.
unsafeFetchCommentPageEntityDB :: CommentId -> DB (Entity WikiPage)
unsafeFetchCommentPageEntityDB = fmap fromJust . fetchCommentPageEntityDB

fetchCommentPageEntityDB :: CommentId -> DB (Maybe (Entity WikiPage))
fetchCommentPageEntityDB comment_id = fmap listToMaybe $
    select $
    from $ \(c `InnerJoin` p) -> do
    on_ (c ^. CommentDiscussion ==. p ^. WikiPageDiscussion)
    where_ (c ^. CommentId ==. val comment_id)
    return p

-- | Get the CommentId this CommentId was rethreaded to, if it was.
fetchCommentRethreadDB :: CommentId -> DB (Maybe CommentId)
fetchCommentRethreadDB comment_id = fmap unValue . listToMaybe <$> (
    select $
    from $ \cr -> do
    where_ $ cr ^. CommentRethreadOldComment ==. val comment_id
    return $ cr ^. CommentRethreadNewComment)

-- | Get a Comment's CommentTags.
fetchCommentCommentTagsDB :: CommentId -> DB [Entity CommentTag]
fetchCommentCommentTagsDB comment_id =
    select $
    from $ \comment_tag -> do
    where_ $ comment_tag ^. CommentTagComment ==. val comment_id
    return comment_tag

-- | Get a Comment's descendants' ids (don't filter hidden or unmoderated comments).
fetchCommentDescendantsIdsDB :: CommentId -> DB [CommentId]
fetchCommentDescendantsIdsDB = fmap (map unValue) . select . querDescendants

-- | Get all descendants of the given root comment.
fetchCommentDescendantsDB :: Maybe UserId -> ProjectId -> CommentId -> DB [Entity Comment]
fetchCommentDescendantsDB mviewer_id project_id root_id =
    select $
    from $ \c -> do
    where_ $
        c ^. CommentId `in_` subList_select (querDescendants root_id) &&.
        exprPermissionFilter mviewer_id (val project_id) c
    -- DO NOT change ordering here! buildCommentTree relies on it.
    orderBy [asc (c ^. CommentParent), asc (c ^. CommentCreatedTs)]
    return c

-- | Get all descendants of all given root comments.
fetchCommentsDescendantsDB :: Maybe UserId -> ProjectId -> [CommentId] -> DB [Entity Comment]
fetchCommentsDescendantsDB mviewer_id project_id root_ids =
    select $
    from $ \c -> do
    where_ $
        c ^. CommentId `in_` subList_select (querAllDescendants root_ids) &&.
        exprPermissionFilter mviewer_id (val project_id) c
    -- DO NOT change ordering here! buildCommentTree relies on it.
    orderBy [asc (c ^. CommentParent), asc (c ^. CommentCreatedTs)]
    return c

-- | Get the "true" target of this CommentId (which may be itself, if not rethreaded -
-- otherwise, ride the rethread train to the end)
fetchCommentDestinationDB :: CommentId -> YDB CommentId
fetchCommentDestinationDB comment_id = do
    void $ get404 comment_id -- make sure the comment even exists, so this function terminates.
    fetchCommentRethreadDB comment_id >>= maybe (return comment_id) fetchCommentDestinationDB

-- | Get all Comments on a Discussion that are root comments.
fetchAllRootCommentsDB :: Maybe UserId -> ProjectId -> DiscussionId -> DB [Entity Comment]
fetchAllRootCommentsDB mviewer_id project_id discussion_id =
    select $
    from $ \c -> do
    where_ $
        exprOnDiscussion discussion_id c &&.
        exprRoot c &&.
        exprPermissionFilter mviewer_id (val project_id) c
    return c

fetchAllClosedRootCommentsDB :: Maybe UserId -> ProjectId -> DiscussionId -> DB [Entity Comment]
fetchAllClosedRootCommentsDB mviewer_id project_id discussion_id =
    select $
    from $ \c -> do
    where_ $
        exprOnDiscussion discussion_id c &&.
        exprRoot c &&.
        exprClosed c &&.
        exprPermissionFilter mviewer_id (val project_id) c
    return c

fetchAllOpenRootCommentsDB :: Maybe UserId -> ProjectId -> DiscussionId -> DB [Entity Comment]
fetchAllOpenRootCommentsDB mviewer_id project_id discussion_id =
    select $
    from $ \c -> do
    where_ $
        exprOnDiscussion discussion_id c &&.
        exprRoot c &&.
        exprOpen c &&.
        exprPermissionFilter mviewer_id (val project_id) c
    return c

-- | Get a Comment's Tags.
fetchCommentTagsDB :: CommentId -> DB [Entity Tag]
fetchCommentTagsDB comment_id =
    select $
    from $ \(ct `InnerJoin` t) -> do
    on_ (ct ^. CommentTagTag ==. t ^. TagId)
    where_ (ct ^. CommentTagComment ==. val comment_id)
    return t

makeClosureMapDB :: (IsList c, CommentId ~ Item c) => c -> DB ClosureMap
makeClosureMapDB comment_ids = fmap (M.fromList . map ((commentClosureComment &&& id) . entityVal)) $
    select $
    from $ \c -> do
    where_ (c ^. CommentClosureComment `in_` valList comment_ids)
    return c

-- Given a collection of CommentId, make a map from CommentId to Entity Ticket. Comments that
-- are not tickets will simply not be in the map.
makeTicketMapDB :: (IsList c, CommentId ~ Item c) => c -> DB TicketMap
makeTicketMapDB comment_ids = fmap (M.fromList . map ((ticketComment . entityVal) &&& id)) $
    select $
    from $ \t -> do
    where_ (t ^. TicketComment `in_` valList comment_ids)
    return t

makeFlagMapDB :: (IsList c, CommentId ~ Item c) => c -> DB FlagMap
makeFlagMapDB comment_ids = mkFlagMap <$> getCommentFlaggings
  where
    getCommentFlaggings :: DB [(CommentId, Maybe Markdown, FlagReason)]
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

rethreadCommentsDB :: RethreadId -> Int -> Maybe CommentId -> DiscussionId -> [CommentId] -> DB [CommentId]
rethreadCommentsDB rethread_id depth_offset maybe_new_parent_id new_discussion_id comment_ids = do
    new_comment_ids <- flip St.evalStateT M.empty $ forM comment_ids $ \ comment_id -> do
        rethreads <- St.get

        Just comment <- get comment_id

        let new_parent_id = maybe maybe_new_parent_id Just $ M.lookup (commentParent comment) rethreads

        new_comment_id <- insert $ comment
            { commentDepth = commentDepth comment - depth_offset
            , commentParent = new_parent_id
            , commentDiscussion = new_discussion_id
            }

        St.put $ M.insert (Just comment_id) new_comment_id rethreads

        return new_comment_id

    forM_ (zip comment_ids new_comment_ids) $ \ (comment_id, new_comment_id) -> do
        update $ \ comment_tag -> do
            where_ $ comment_tag ^. CommentTagComment ==. val comment_id
            set comment_tag [ CommentTagComment =. val new_comment_id ]

        update $ \ ticket -> do
            where_ $ ticket ^. TicketComment ==. val comment_id
            set ticket [ TicketComment =. val new_comment_id ]

        insert_ $ CommentRethread rethread_id comment_id new_comment_id

    insertSelect $
        from $ \(comment_closure `InnerJoin` comment_rethread) -> do
        on_ $ comment_closure ^. CommentClosureComment ==. comment_rethread ^. CommentRethreadOldComment
        return $ CommentClosure
                    <#  (comment_closure ^. CommentClosureTs)
                    <&> (comment_closure ^. CommentClosureClosedBy)
                    <&> (comment_closure ^. CommentClosureType)
                    <&> (comment_closure ^. CommentClosureReason)
                    <&> (comment_rethread ^. CommentRethreadNewComment)

    return new_comment_ids
