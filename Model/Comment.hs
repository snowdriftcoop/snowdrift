module Model.Comment
    -- Types
    ( ClosureMap
    , CommentActionPermissions(..)
    , CommentMods(..)
    , CommentRoutes(..)
    , FlagMap
    , MaxDepth(..)
    , NoCommentReason(..)
    , TicketMap
    , addMaxDepth
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
    , fetchCommentAncestorClosuresDB
    , fetchCommentAncestorClosuresDB'
    , fetchCommentsAncestorClosuresDB
    , fetchCommentDB
    , fetchCommentAllDescendantsDB
    , fetchCommentAncestorsDB
    , fetchCommentCommentTagsDB
    , fetchCommentCommentTagsInDB
    , fetchCommentDepthDB
    , fetchCommentDepth404DB
    , fetchCommentDescendantsDB
    , fetchCommentDestinationDB
    , fetchCommentFlaggingDB
    , fetchCommentsDescendantsDB
    , fetchCommentWikiPageDB
    , fetchCommentRethreadDB
    , fetchCommentTagsDB
    , fetchCommentTagCommentTagsDB
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
import           Model.Notification
import           Model.Tag

import qualified Control.Monad.State         as St
import           Control.Monad.Writer.Strict (tell)
import           Data.Default                (Default, def)
import           Data.Foldable               (Foldable)
import qualified Data.Foldable               as F
import qualified Data.Map                    as M
import           Data.Maybe                  (fromJust)
import qualified Data.Set                    as S
import qualified Data.Text                   as T
import           Data.Tree
import qualified Database.Persist            as P
import           GHC.Exts                    (IsList(..))
import           Yesod.Markdown              (Markdown(..))

--------------------------------------------------------------------------------
-- Types

type ClosureMap = Map CommentId CommentClosure
type TicketMap  = Map CommentId (Entity Ticket)
type FlagMap    = Map CommentId (Maybe Markdown, [FlagReason])

-- | A root comment (with its own URL) might not be displayed. Why?
data NoCommentReason
    = CommentNotFound
    | CommentPermissionDenied

-- | Collection of Routes that can be made from a CommentId (because Comments on
-- various places around the site have different URLs, even though their
-- IDs are unique).
data CommentRoutes = CommentRoutes
    { comment_route_add_tag   :: CommentId -> Route App
    , comment_route_approve   :: CommentId -> Route App
    , comment_route_close     :: CommentId -> Route App
    , comment_route_delete    :: CommentId -> Route App
    , comment_route_edit      :: CommentId -> Route App
    , comment_route_flag      :: CommentId -> Route App
    , comment_route_permalink :: CommentId -> Route App
    , comment_route_reply     :: CommentId -> Route App
    , comment_route_rethread  :: CommentId -> Route App
    , comment_route_retract   :: CommentId -> Route App
    , comment_route_tag       :: CommentId -> TagId -> Route App
    }

-- | Data type used in makeCommentWidgetMod, containing modifications to comment-action-related
-- data structures.
data CommentMods = CommentMods
    { mod_comment          :: Comment          -> Comment
    , mod_earlier_closures :: [CommentClosure] -> [CommentClosure]
    , mod_user_map         :: Map UserId User  -> Map UserId User -- can't user UserMap here, circular dependency.
    , mod_closure_map      :: ClosureMap       -> ClosureMap
    , mod_ticket_map       :: TicketMap        -> TicketMap
    , mod_flag_map         :: FlagMap          -> FlagMap
    , mod_tag_map          :: TagMap           -> TagMap
    }

instance Default CommentMods where
    def = CommentMods id id id id id id id

data CommentActionPermissions = CommentActionPermissions
    { can_add_tag   :: Bool
    , can_approve   :: Bool
    , can_close     :: Bool
    , can_delete    :: Bool
    , can_edit      :: Bool
    , can_establish :: Bool
    , can_flag      :: Bool
    , can_reply     :: Bool
    , can_rethread  :: Bool
    , can_retract   :: Bool
    }

data MaxDepth
    = NoMaxDepth
    | MaxDepth Int

instance Eq MaxDepth where
    NoMaxDepth == NoMaxDepth = True
    MaxDepth x == MaxDepth y = x == y
    _          == _          = False

instance Ord MaxDepth where
    compare NoMaxDepth   (MaxDepth _) = GT
    compare NoMaxDepth   NoMaxDepth   = EQ
    compare (MaxDepth _) NoMaxDepth   = LT
    compare (MaxDepth x) (MaxDepth y) = compare x y

addMaxDepth :: MaxDepth -> Int -> MaxDepth
addMaxDepth NoMaxDepth   _ = NoMaxDepth
addMaxDepth (MaxDepth x) y = MaxDepth (x + y)

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
buildCommentTree :: Entity Comment -> [Entity Comment] -> Tree (Entity Comment)
buildCommentTree r rs = unfoldTree step (r,rs)
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

buildCommentForest :: [Entity Comment] -- root comments
                   -> [Entity Comment] -- replies comments
                   -> Forest (Entity Comment)
buildCommentForest roots replies = (map (flip buildCommentTree replies)) roots

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
    now <- liftIO getCurrentTime
    -- Do an in-memory adjustment of the comment with exactly the same changes
    -- as 'upd' below (so we can avoid hitting the database).
    let updated_comment = comment
          { commentModeratedTs = Just now
          , commentModeratedBy = Just user_id
          }
    lift $ do
        updateComment now
        deleteUnapprovedCommentNotifications
    tell [ECommentPosted comment_id updated_comment]
  where
    updateComment now =
        update $ \c -> do
        set c [ CommentModeratedTs =. val (Just now)
              , CommentModeratedBy =. val (Just user_id)
              ]
        where_ (c ^. CommentId ==. val comment_id)

    -- Delete all notifications sent about this pending comment, as they no longer apply.
    -- Also deletes the UnapprovedCommentNotification entities, the EventNotificationSent,
    -- and any other rows with a foreign key on NotificationId.
    deleteUnapprovedCommentNotifications = do
        notif_ids <- fmap (map unValue) $
                         select $
                         from $ \unc -> do
                         where_ (unc ^. UnapprovedCommentNotificationComment ==. val comment_id)
                         return (unc ^. UnapprovedCommentNotificationNotification)
        deleteCascadeWhere [NotificationId P.<-. notif_ids]

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

-- | Fetch a comment from the DB, subject to viewing permissions.
fetchCommentDB :: CommentId -> ExprCommentCond -> DB (Either NoCommentReason Comment)
fetchCommentDB comment_id has_permission = get comment_id >>= \case
    Nothing -> return (Left CommentNotFound)
    -- Hooray, the comment exists, now toss it and re-query the database with the
    -- provided permission conditions. How else would we be able to differentiate
    -- a non-existent comment and a comment the user doesn't have permission to
    -- view?
    Just _ -> fmap (maybe (Left CommentPermissionDenied) (Right . entityVal) . listToMaybe) $
                  select $
                  from $ \c -> do
                  where_ (has_permission c)
                  return c

-- | Delete-cascade a comment from the database.
deleteCommentDB :: CommentId -> DB ()
deleteCommentDB = deleteCascade

-- | Edit a comment's text. If the comment was flagged, unflag it and send a
-- notification to the flagger.
editCommentDB :: CommentId -> Markdown -> SYDB ()
editCommentDB comment_id text = do
    lift updateCommentText
    lift (fetchCommentFlaggingDB comment_id) >>= \case
        Nothing -> return ()
        Just (Entity comment_flagging_id CommentFlagging{..}) -> do
            permalink_text <- lift (getUrlRender <*> pure (CommentDirectLinkR comment_id))
            let notif_text = Markdown $ "A comment you flagged has been edited and reposted to the site. You can view it [here](" <> permalink_text <> ")."
            lift (deleteCascade comment_flagging_id) -- delete flagging and all flagging reasons with it.
            sendNotificationDB_ NotifFlagRepost commentFlaggingFlagger Nothing notif_text
  where
    updateCommentText =
        update $ \c -> do
        set c [ CommentText =. val text ]
        where_ (c ^. CommentId ==. val comment_id)

-- | Flag a comment. Send a notification to the poster about the flagging. Return whether
-- or not the flag was successful (fails if the comment was already flagged.)
flagCommentDB :: CommentId -> Text -> UserId -> [FlagReason] -> Maybe Markdown -> SYDB Bool
flagCommentDB comment_id permalink_route flagger_id reasons message = do
    poster_id <- lift (commentUser <$> get404 comment_id)
    now <- liftIO getCurrentTime
    lift (insertUnique (CommentFlagging now flagger_id comment_id message)) >>= \case
        Nothing -> return False
        Just flagging_id -> do
            lift $ void $ insertMany (map (CommentFlaggingReason flagging_id) reasons)

            let notif_text = Markdown . T.unlines $
                    [ "Another user flagged your comment as not meeting the standards of the Code of Conduct. We *want* your involvement as long as it remains respectful and friendly, so please don’t feel discouraged."
                    , ""
                    , "Please follow the link below for clarification and suggestions the flagger may have offered, and take this chance to improve your tone and clarify any misunderstanding. Your newly edited comment will then be publicly visible again."
                    , ""
                    , "Please alert a moderator if you believe that this flagging is inappropriate, if the flagger violated the Code of Conduct in their feedback, or if you want other assistance."
                    , ""
                    , "[link to flagged comment](" <> permalink_route <> ")"
                    ]
            sendNotificationDB_ NotifFlag poster_id Nothing notif_text
            return True

-- | Filter a list of comments per the provided permission filter.
filterCommentsDB :: [CommentId] -> ExprCommentCond -> DB [CommentId]
filterCommentsDB comment_ids has_permission = fmap (map unValue) $
    select $
    from $ \c -> do
    where_ $
        c ^. CommentId `in_` valList comment_ids &&.
        has_permission c
    return (c ^. CommentId)

-- | Get all ancestors that have been closed.
fetchCommentAncestorClosuresDB :: CommentId -> DB [CommentClosure]
fetchCommentAncestorClosuresDB comment_id = fmap (map entityVal) $
    select $
    from $ \(ca `InnerJoin` cc) -> do
    on_ (ca ^. CommentAncestorAncestor ==. cc ^. CommentClosureComment)
    orderBy [asc (cc ^. CommentClosureComment)]
    where_ (ca ^. CommentAncestorComment ==. val comment_id)
    return cc

-- | Get all ancestors, including this comment, that have been closed.
fetchCommentAncestorClosuresDB' :: CommentId -> DB [CommentClosure]
fetchCommentAncestorClosuresDB' comment_id = do
    all_comment_ids <- (comment_id :) <$> fetchCommentAncestorsDB comment_id
    fmap (map entityVal) $
        select $
        from $ \cc -> do
        where_ (cc ^. CommentClosureComment `in_` valList all_comment_ids)
        return cc

-- | Get all CommentClosures of any of the given Comments' ancestors, grouped by
-- the given Comments.
fetchCommentsAncestorClosuresDB :: [CommentId] -> DB (Map CommentId [CommentClosure])
fetchCommentsAncestorClosuresDB comment_ids = fmap (foldr step mempty) $
    select $
    from $ \(ca `InnerJoin` cc) -> do
    on_ (ca ^. CommentAncestorAncestor ==. cc ^. CommentClosureComment)
    orderBy [asc (cc ^. CommentClosureComment)]
    where_ (ca ^. CommentAncestorComment `in_` valList comment_ids)
    return (ca ^. CommentAncestorComment, cc)
  where
    step :: (Value CommentId, Entity CommentClosure) -> Map CommentId [CommentClosure] -> Map CommentId [CommentClosure]
    step (Value c, Entity _ cc) = M.insertWith (++) c [cc]

-- | Get a comment's ancestors' ids.
fetchCommentAncestorsDB :: CommentId -> DB [CommentId]
fetchCommentAncestorsDB = fmap (map unValue) . select . querCommentAncestors

subFetchCommentAncestorsDB :: CommentId -> SqlExpr (ValueList CommentId)
subFetchCommentAncestorsDB = subList_select . querCommentAncestors

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
unsafeFetchCommentPageEntityDB = fmap fromJust . fetchCommentWikiPageDB

fetchCommentWikiPageDB :: CommentId -> DB (Maybe (Entity WikiPage))
fetchCommentWikiPageDB comment_id = fmap listToMaybe $
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
fetchCommentCommentTagsDB :: CommentId -> DB [CommentTag]
fetchCommentCommentTagsDB comment_id = fmap (map entityVal) $
    select $
    from $ \ct -> do
    where_ (ct ^. CommentTagComment ==. val comment_id)
    return ct

fetchCommentCommentTagsInDB :: [CommentId] -> DB [CommentTag]
fetchCommentCommentTagsInDB comment_ids = fmap (map entityVal) $
    select $
    from $ \ct -> do
    where_ (ct ^. CommentTagComment `in_` valList comment_ids)
    return ct

-- | Get a Comment's descendants' ids (don't filter hidden or unmoderated comments).
fetchCommentAllDescendantsDB :: CommentId -> DB [CommentId]
fetchCommentAllDescendantsDB = fmap (map unValue) . select . querCommentDescendants

-- | Get all descendants of the given root comment.
fetchCommentDescendantsDB :: CommentId -> ExprCommentCond -> DB [Entity Comment]
fetchCommentDescendantsDB comment_id has_permission =
    select $
    from $ \c -> do
    where_ $
        c ^. CommentId `in_` subList_select (querCommentDescendants comment_id) &&.
        has_permission c
    -- DO NOT change ordering here! buildCommentTree relies on it.
    orderBy [asc (c ^. CommentParent), asc (c ^. CommentCreatedTs)]
    return c

-- | Get all descendants of all given root comments.
fetchCommentsDescendantsDB :: [CommentId] -> ExprCommentCond -> DB [Entity Comment]
fetchCommentsDescendantsDB comment_ids has_permission =
    select $
    from $ \c -> do
    where_ $
        c ^. CommentId `in_` subList_select (querCommentsDescendants comment_ids) &&.
        has_permission c
    -- DO NOT change ordering here! buildCommentTree relies on it.
    orderBy [asc (c ^. CommentParent), asc (c ^. CommentCreatedTs)]
    return c

-- | Get the "true" target of this CommentId (which may be itself, if not rethreaded -
-- otherwise, ride the rethread train to the end)
fetchCommentDestinationDB :: CommentId -> YDB CommentId
fetchCommentDestinationDB comment_id = do
    void $ get404 comment_id -- make sure the comment even exists, so this function terminates.
    fetchCommentRethreadDB comment_id >>= maybe (return comment_id) fetchCommentDestinationDB

-- | Get a Comment's Tags.
fetchCommentTagsDB :: CommentId -> DB [Entity Tag]
fetchCommentTagsDB comment_id =
    select $
    from $ \(ct `InnerJoin` t) -> do
    on_ (ct ^. CommentTagTag ==. t ^. TagId)
    where_ (ct ^. CommentTagComment ==. val comment_id)
    return t

-- | Get a Comment's CommentTags for a specific Tag.
fetchCommentTagCommentTagsDB :: CommentId -> TagId -> DB [CommentTag]
fetchCommentTagCommentTagsDB comment_id tag_id = fmap (map entityVal) $
    select $
    from $ \ct -> do
    where_ $
        ct ^. CommentTagComment ==. val comment_id &&.
        ct ^. CommentTagTag ==. val tag_id
    return ct

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
