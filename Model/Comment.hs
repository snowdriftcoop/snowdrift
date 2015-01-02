module Model.Comment
    -- Types
    ( MaxDepth(..)
    , NoCommentReason(..)
    , addMaxDepth
    -- Utility functions
    , buildCommentForest
    , buildCommentTree
    , commentIsApproved
    , commentIsEvenDepth
    , commentIsFlagged
    , commentIsOddDepth
    , commentIsPrivate
    , commentIsTopLevel
    , makeCommentUsersSet
    , makeApprovedComment
    -- Database actions
    , approveCommentDB
    , deleteCommentDB
    , editCommentDB
    , flagCommentDB
    , fetchCommentAncestorClosuresDB
    , fetchCommentAncestorRetractsDB
    , fetchCommentAncestorClosuresDB'
    , fetchCommentAncestorRetractsDB'
    , fetchCommentsAncestorClosuresDB
    , fetchCommentsAncestorRetractsDB
    , fetchCommentDB
    , fetchCommentAllCurrentDescendantsDB
    , fetchCommentAllDescendantsDB
    , fetchCommentAncestorsDB
    , fetchCommentCommentTagsDB
    , fetchCommentCommentTagsInDB
    , fetchCommentDepthDB
    , fetchCommentDepthFromMaybeParentIdDB
    , fetchCommentDepth404DB
    , fetchCommentDescendantsDB
    , fetchCommentDestinationDB
    , fetchCommentFlaggingDB
    , fetchCommentRethreadDB
    , fetchCommentTagsDB
    , fetchCommentTagCommentTagsDB
    , fetchCommentsDescendantsDB
    , fetchCommentsInDB
    , fetchCommentsWithChildrenInDB
    , fetchCommentTicketsDB
    , filterCommentsDB
    , insertTagsDB
    , insertTicketsDB
    , makeClaimedTicketMapDB
    , makeCommentClosingMapDB
    , makeCommentRetractingMapDB
    , makeCommentRouteDB
    , makeFlagMapDB
    , makeTicketMapDB
    , makeWatchMapDB
    , postApprovedCommentDB
    , postUnapprovedCommentDB
    , rethreadCommentDB
    , subFetchCommentAncestorsDB
    ) where

import Import

import Model.Comment.Sql
import Model.Discussion
import Model.Notification
import Model.User.Internal (sendPreferredNotificationDB)

import qualified Control.Monad.State                  as State
import           Control.Monad.Writer.Strict          (tell)
import           Data.Foldable                        (Foldable)
import qualified Data.Foldable                        as F
import qualified Data.Map                             as M
import           Data.Maybe                           (fromJust)
import qualified Data.Set                             as S
import qualified Data.Text                            as T
import           Data.Tree
import           Database.Esqueleto.Internal.Language (Insertion)
import qualified Database.Persist                     as P
import           GHC.Exts                             (IsList(..))
import qualified Prelude                              as Prelude
import           Yesod.Markdown                       (Markdown(..))

--------------------------------------------------------------------------------
-- Types

-- | A root comment (with its own URL) might not be displayed. Why?
data NoCommentReason
    = CommentNotFound
    | CommentPermissionDenied

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
commentIsApproved = isJust . commentApprovedTs

commentIsTopLevel :: Comment -> Bool
commentIsTopLevel = (== 0) . commentDepth

commentIsEvenDepth :: Comment -> Bool
commentIsEvenDepth comment = not (commentIsTopLevel comment) && commentDepth comment `mod` 2 == 1

commentIsOddDepth :: Comment -> Bool
commentIsOddDepth comment = not (commentIsTopLevel comment) && not (commentIsEvenDepth comment)

commentIsFlagged :: CommentId -> DB Bool
commentIsFlagged = fmap (maybe False (const True)) . getBy . UniqueCommentFlagging

commentIsPrivate :: Comment -> Bool
commentIsPrivate comment = commentVisibility comment == VisPrivate

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

-- newClosedCommentClosure, newRetractedCommentClosure :: MonadIO m => UserId -> Markdown -> CommentId -> m CommentClosure
-- newClosedCommentClosure    = newCommentClosure Closed
-- newRetractedCommentClosure = newCommentClosure Retracted

-- newCommentClosure :: MonadIO m => ClosureType -> UserId -> Markdown -> CommentId -> m CommentClosure
-- newCommentClosure closure_type user_id reason comment_id =
--     (\now -> CommentClosure now user_id closure_type reason comment_id) `liftM` liftIO getCurrentTime

-- | Construct a comment, auto-approved by 'this' User (because they are established).
makeApprovedComment :: UserId -> DiscussionId -> Maybe CommentId -> Markdown -> Int -> Visibility -> Language -> YDB Comment
makeApprovedComment user_id discussion_id parent_comment comment_text depth visibility language = do
    now <- liftIO getCurrentTime
    maybe_parent_visibility <- case parent_comment of
        Nothing -> return Nothing
        Just parent_comment_id -> fmap (fmap commentVisibility) $ get parent_comment_id

    let parent_visibility = fromMaybe VisPublic maybe_parent_visibility

    return $ Comment
                 now
                 (Just now)
                 (Just user_id)
                 discussion_id
                 parent_comment
                 user_id
                 comment_text
                 depth
                 (min visibility parent_visibility)
                 language

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
          { commentApprovedTs = Just now
          , commentApprovedBy = Just user_id
          }
    lift $ do
        updateComment now
        deleteUnapprovedCommentNotifications
    tell [ ECommentPosted comment_id updated_comment
         , ECommentApproved comment_id updated_comment
         ]
  where
    updateComment now =
        update $ \c -> do
        set c [ CommentApprovedTs =. val (Just now)
              , CommentApprovedBy =. val (Just user_id)
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

insertApprovedCommentDB
        :: UTCTime
        -> DiscussionId
        -> Maybe CommentId
        -> UserId
        -> Markdown
        -> Int
        -> Visibility
        -> Language
        -> SDB CommentId
insertApprovedCommentDB created_ts discussion_id mparent_id user_id text depth visibility language =
    insertCommentDB
        (Just created_ts)
        (Just user_id)
        ECommentPosted
        created_ts
        discussion_id
        mparent_id
        user_id
        text
        depth
        visibility
        language

insertUnapprovedCommentDB
        :: UTCTime
        -> DiscussionId
        -> Maybe CommentId
        -> UserId
        -> Markdown
        -> Int
        -> Visibility
        -> Language
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
                -> Visibility
                -> Language
                -> SDB CommentId
insertCommentDB mapproved_ts mapproved_by mk_event created_ts discussion_id mparent_id user_id text depth visibility language = do
    mparent <- case mparent_id of
        Nothing -> return Nothing
        Just parent_id -> get parent_id

    let parent_visibility = maybe VisPublic commentVisibility mparent
        comment = Comment
                    created_ts
                    mapproved_ts
                    mapproved_by
                    discussion_id
                    mparent_id
                    user_id
                    text
                    depth
                    (min visibility parent_visibility)
                    language

    comment_id <- lift $ insert comment
    tell [mk_event comment_id comment]
    return comment_id

-- | Fetch a comment from the DB, subject to viewing permissions.
fetchCommentDB :: CommentId -> ExprCommentCond -> DB (Either NoCommentReason Comment)
fetchCommentDB comment_id has_permission = get comment_id >>= \case
    Nothing -> do
        liftIO $ appendFile "log" $ "comment not found: " ++ show comment_id ++ "\n"
        return (Left CommentNotFound)
    -- Hooray, the comment exists, now toss it and re-query the database with the
    -- provided permission conditions. How else would we be able to differentiate
    -- a non-existent comment and a comment the user doesn't have permission to
    -- view?
    Just _ -> fmap (maybe (Left CommentPermissionDenied) (Right . entityVal) . listToMaybe) $
                  select $
                  from $ \c -> do
                  where_ $
                      c ^. CommentId ==. val comment_id &&.
                      has_permission c
                  return c

fetchCommentsInDB :: [CommentId] -> ExprCommentCond -> DB [Entity Comment]
fetchCommentsInDB comment_ids has_permission =
    select $
    from $ \c -> do
    where_ $
        c ^. CommentId `in_` valList comment_ids &&.
        has_permission c
    return c

-- | Delete-cascade a comment from the database.
deleteCommentDB :: CommentId -> DB ()
deleteCommentDB = deleteCascade

-- | Edit a comment's text. If the comment was flagged, unflag it and send a
-- notification to the flagger.
editCommentDB :: CommentId -> Markdown -> Language -> SYDB ()
editCommentDB comment_id text language = do
    lift updateComment
    lift (fetchCommentFlaggingDB comment_id) >>= \case
        Nothing -> return ()
        Just (Entity comment_flagging_id CommentFlagging{..}) -> do
            langs <- lift $ lift getLanguages
            render <- getUrlRender
            rendered_route <- lift $ makeCommentRouteDB langs comment_id >>= return . render . fromJust
            let notif_text = Markdown $ "A comment you flagged has been edited and reposted to the site. You can view it [here](" <> rendered_route <> ")."
            lift (deleteCascade comment_flagging_id) -- delete flagging and all flagging reasons with it.
            sendPreferredNotificationDB commentFlaggingFlagger NotifFlagRepost Nothing Nothing notif_text
  where
    updateComment =
        update $ \c -> do
        set c [ CommentText     =. val text
              , CommentLanguage =. val language
              ]
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
            sendPreferredNotificationDB poster_id NotifFlag Nothing Nothing notif_text
            return True

-- | Post an new (approved) Comment.
postApprovedCommentDB :: UserId -> Maybe CommentId -> DiscussionId -> Markdown -> Visibility -> Language -> SDB CommentId
postApprovedCommentDB = postComment insertApprovedCommentDB

postUnapprovedCommentDB :: UserId -> Maybe CommentId -> DiscussionId -> Markdown -> Visibility -> Language -> SDB CommentId
postUnapprovedCommentDB = postComment insertUnapprovedCommentDB

tickets :: Text -> [Text]
tickets = filter (not . T.null) . map T.strip
        . mapMaybe (T.stripPrefix "ticket:") . T.lines

insertTicketsDB :: UTCTime -> CommentId -> Text -> DB ()
insertTicketsDB now comment_id content =
    forM_ (tickets content) $ \ticket ->
        insert_ $ Ticket now now ticket comment_id

tags :: Text -> [Text]
tags = filter (not . T.null) . map T.strip . mconcat . map (T.splitOn ",")
     . mapMaybe (T.stripPrefix "tags:") . T.lines

insertTagsDB :: UserId -> CommentId -> Text -> DB ()
insertTagsDB user_id comment_id content =
    forM_ (tags content) $ \tag -> do
        tag_id <- either entityKey id <$> insertBy (Tag tag)
        insert_ $ CommentTag comment_id tag_id user_id 1

postComment
        :: (UTCTime -> DiscussionId -> Maybe CommentId -> UserId -> Markdown -> Int -> Visibility -> Language -> SDB CommentId)
        -> UserId
        -> Maybe CommentId
        -> DiscussionId
        -> Markdown
        -> Visibility
        -> Language
        -> SDB CommentId
postComment insert_comment user_id mparent_id discussion_id contents visibility language = do
    (now, depth) <- lift $ (,)
        <$> liftIO getCurrentTime
        <*> fetchCommentDepthFromMaybeParentIdDB mparent_id

    comment_id <- insert_comment now discussion_id mparent_id user_id contents depth visibility language

    let content = unMarkdown contents

    lift $ do
        insertTicketsDB now comment_id content
        insertTagsDB user_id comment_id content

        case mparent_id of
            Nothing -> return ()
            Just parent_id -> mapM_ (insert_ . CommentAncestor comment_id) =<< (parent_id:) <$> fetchCommentAncestorsDB parent_id

        update $ \t -> do
         set t [TicketUpdatedTs =. val now]
         where_ (t ^. TicketComment `in_` subFetchCommentAncestorsDB comment_id)

    return comment_id

-- | Filter a list of comments per the provided permission filter.
filterCommentsDB :: [CommentId] -> ExprCommentCond -> DB [CommentId]
filterCommentsDB comment_ids has_permission = fmap (map unValue) $
    select $
    from $ \c -> do
    where_ $
        c ^. CommentId `in_` valList comment_ids &&.
        has_permission c
    return (c ^. CommentId)

-- | Get all ancestors that have been closed/retracted.
fetchCommentAncestorClosuresDB :: CommentId -> DB [CommentClosing]
fetchCommentAncestorRetractsDB :: CommentId -> DB [CommentRetracting]
fetchCommentAncestorClosuresDB = commentClosuresOrRetracts CommentClosingComment
fetchCommentAncestorRetractsDB = commentClosuresOrRetracts CommentRetractingComment

commentClosuresOrRetracts :: (PersistEntity val, PersistEntityBackend val ~ SqlBackend)
                          => (EntityField val CommentId) -> CommentId -> DB [val]
commentClosuresOrRetracts comment_field comment_id = fmap (map entityVal) $
    select $
    from $ \(ca `InnerJoin` table) -> do
    on_ (ca ^. CommentAncestorAncestor ==. table ^. comment_field)
    orderBy [asc (table ^. comment_field)]
    where_ (ca ^. CommentAncestorComment ==. val comment_id)
    return table

-- | Get all ancestors, including this comment, that have been closed/retracted.
fetchCommentAncestorClosuresDB' :: CommentId -> DB [CommentClosing]
fetchCommentAncestorRetractsDB' :: CommentId -> DB [CommentRetracting]
fetchCommentAncestorClosuresDB' = commentClosuresOrRetracts' CommentClosingComment
fetchCommentAncestorRetractsDB' = commentClosuresOrRetracts' CommentRetractingComment

commentClosuresOrRetracts' :: (PersistEntity val, PersistEntityBackend val ~ SqlBackend)
                           => (EntityField val CommentId) -> CommentId -> DB [val]
commentClosuresOrRetracts' comment_field comment_id = do
    all_comment_ids <- (comment_id :) <$> fetchCommentAncestorsDB comment_id
    fmap (map entityVal) $
        select $
        from $ \table -> do
        where_ (table ^. comment_field `in_` valList all_comment_ids)
        return table

-- | Get all CommentClosings/CommentRetracts of any of the given Comments' ancestors, grouped by
-- the given Comments.
fetchCommentsAncestorClosuresDB :: [CommentId] -> DB (Map CommentId [CommentClosing])
fetchCommentsAncestorRetractsDB :: [CommentId] -> DB (Map CommentId [CommentRetracting])
fetchCommentsAncestorClosuresDB = commentsClosuresOrRetracts CommentClosingComment
fetchCommentsAncestorRetractsDB = commentsClosuresOrRetracts CommentRetractingComment

commentsClosuresOrRetracts :: (PersistEntity val, PersistEntityBackend val ~ SqlBackend)
                           => EntityField val CommentId -> [CommentId] -> DB (Map CommentId [val])
commentsClosuresOrRetracts comment_field comment_ids = fmap (foldr step mempty) $
    select $
    from $ \(ca `InnerJoin` table) -> do
    on_ (ca ^. CommentAncestorAncestor ==. table ^. comment_field)
    orderBy [asc (table ^. comment_field)]
    where_ (ca ^. CommentAncestorComment `in_` valList comment_ids)
    return (ca ^. CommentAncestorComment, table)
  where
    step (Value c, Entity _ v) = M.insertWith (++) c [v]

-- | Get a comment's ancestors' ids.
fetchCommentAncestorsDB :: CommentId -> DB [CommentId]
fetchCommentAncestorsDB = fmap (map unValue) . select . querCommentAncestors

subFetchCommentAncestorsDB :: CommentId -> SqlExpr (ValueList CommentId)
subFetchCommentAncestorsDB = subList_select . querCommentAncestors

fetchCommentDepthDB :: CommentId -> DB Int
fetchCommentDepthDB = fmap commentDepth . getJust

-- | Get the depth of a comment, given (maybe) its parent's CommentId.
fetchCommentDepthFromMaybeParentIdDB :: Maybe CommentId -> DB Int
fetchCommentDepthFromMaybeParentIdDB = maybe (return 0) (fmap (+1) . fetchCommentDepthDB)

fetchCommentDepth404DB :: CommentId -> Handler Int
fetchCommentDepth404DB = fmap commentDepth . runYDB . get404

-- | Get the CommentFlagging even for this Comment, if there is one.
fetchCommentFlaggingDB :: CommentId -> DB (Maybe (Entity CommentFlagging))
fetchCommentFlaggingDB = getBy . UniqueCommentFlagging

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

-- | Get a Comment's descendants' ids (don't filter hidden or unapproved comments).
fetchCommentAllCurrentDescendantsDB :: CommentId -> DB [CommentId]
fetchCommentAllCurrentDescendantsDB comment_id = fmap (map unValue) $
    select $ from $ \ ca -> do
        where_ $ ca ^. CommentAncestorAncestor ==. val comment_id
            &&. ca ^. CommentAncestorComment `notIn` (subList_select $ from $ return . (^. CommentRethreadOldComment))
        orderBy [asc (ca ^. CommentAncestorComment)]
        return (ca ^. CommentAncestorComment)

-- | Get a Comment's descendants' ids (don't filter hidden or unapproved comments).
fetchCommentAllDescendantsDB :: CommentId -> DB [CommentId]
fetchCommentAllDescendantsDB comment_id = fmap (map unValue) $
    select $
    from $ \ca -> do
    where_ (ca ^. CommentAncestorAncestor ==. val comment_id)
    orderBy [asc (ca ^. CommentAncestorComment)]
    return (ca ^. CommentAncestorComment)

-- | Get all descendants of the given root comment.
fetchCommentDescendantsDB :: CommentId -> ExprCommentCond -> DB [Entity Comment]
fetchCommentDescendantsDB comment_id has_permission =
    select $
    from $ \c -> do
    where_ $
        has_permission c &&.
        c ^. CommentId `in_` (subList_select $
                              from $ \ca -> do
                              where_ (ca ^. CommentAncestorAncestor ==. val comment_id)
                              return (ca ^. CommentAncestorComment))
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

-- | Given a list of candidate CommentIds, return only those that have any
-- (possibly not visible) children.
fetchCommentsWithChildrenInDB :: [CommentId] -> DB [CommentId]
fetchCommentsWithChildrenInDB comment_ids = fmap (map unValue) $
    selectDistinct $
    from $ \ca -> do
    where_ (ca ^. CommentAncestorAncestor `in_` valList comment_ids)
    return (ca ^. CommentAncestorAncestor)

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

makeCommentClosingMapDB    :: (IsList c, CommentId ~ Item c) => c -> DB (Map CommentId CommentClosing)
makeCommentRetractingMapDB :: (IsList c, CommentId ~ Item c) => c -> DB (Map CommentId CommentRetracting)
makeCommentClosingMapDB    = closeOrRetractMap CommentClosingComment    commentClosingComment
makeCommentRetractingMapDB = closeOrRetractMap CommentRetractingComment commentRetractingComment

closeOrRetractMap
        :: (IsList c, CommentId ~ Item c, PersistEntity val, PersistEntityBackend val ~ SqlBackend)
        => EntityField val CommentId
        -> (val -> CommentId)
        -> c
        -> DB (Map CommentId val)
closeOrRetractMap comment_field comment_projection comment_ids = fmap (foldr step mempty) $
    select $
    from $ \c -> do
    where_ (c ^. comment_field `in_` valList comment_ids)
    return c
  where
    -- step :: Entity val -> Map CommentId val -> Map CommentId val
    step (Entity _ c) = M.insert (comment_projection c) c

-- | Given a collection of CommentId, make a map from CommentId to Entity Ticket. Comments that
-- are not tickets will simply not be in the map.
makeTicketMapDB :: (IsList c, CommentId ~ Item c) => c -> DB (Map CommentId (Entity Ticket))
makeTicketMapDB comment_ids = fmap (foldr step mempty) $
    select $
    from $ \t -> do
    where_ (t ^. TicketComment `in_` valList comment_ids)
    return t
  where
    step t = M.insert (ticketComment (entityVal t)) t

makeClaimedTicketMapDB :: [CommentId] -> DB (Map CommentId TicketClaiming)
makeClaimedTicketMapDB comment_ids = fmap (M.fromList . map (\(Value x, Entity _ y) -> (x, y))) $
    select $
    from $ \tc -> do
    where_ (tc ^. TicketClaimingTicket `in_` valList comment_ids)
    return (tc ^. TicketClaimingTicket, tc)

-- | Given a collection of CommentId, make a flag map. Comments that are not flagged
-- will simply not be in the map.
makeFlagMapDB :: (IsList c, CommentId ~ Item c) => c -> DB (Map CommentId (CommentFlagging, [FlagReason]))
makeFlagMapDB comment_ids = fmap (go . map (\(Entity _ x, Value y) -> (x, y))) $
    select $
    from $ \(cf `InnerJoin` cfr) -> do
    on_ (cf ^. CommentFlaggingId ==. cfr ^. CommentFlaggingReasonFlagging)
    where_ (cf ^. CommentFlaggingComment `in_` valList comment_ids)
    return (cf, cfr ^. CommentFlaggingReasonReason)
  where
    go :: [(CommentFlagging, FlagReason)] -> Map CommentId (CommentFlagging, [FlagReason])
    go = foldr (\(cf@(CommentFlagging _ _ comment_id _), reason) -> M.insertWith combine comment_id (cf, [reason])) mempty
      where
        combine :: (CommentFlagging, [FlagReason]) -> (CommentFlagging, [FlagReason]) -> (CommentFlagging, [FlagReason])
        combine (cf, reasons1) (_, reasons2) = (cf, reasons1 <> reasons2)

-- | Given a collection of CommentId, make a map from comment ids to sets of watches. Comments that are not watched
-- will simply not be in the map.
--
-- TODO: Return enough info to link to the root of the watch
makeWatchMapDB :: (IsList c, CommentId ~ Item c) => c -> DB (Map CommentId (Set WatchedSubthread))
makeWatchMapDB comment_ids = fmap (M.fromListWith mappend . map (\(Value x, Entity _ y) -> (x, S.singleton y))) $ do
    ancestral_watches <- select $ from $ \ (ws `InnerJoin` ca) -> do
        on_ $ ws ^. WatchedSubthreadRoot ==. ca ^. CommentAncestorAncestor
        where_ $ ca ^. CommentAncestorComment `in_` valList comment_ids
        return (ca ^. CommentAncestorComment, ws)

    current_watches <- select $ from $ \ ws -> do
        where_ $ ws ^. WatchedSubthreadRoot `in_` valList comment_ids
        return (ws ^. WatchedSubthreadRoot, ws)

    return $ ancestral_watches <> current_watches

rethreadCommentDB :: Maybe CommentId -> DiscussionId -> CommentId -> UserId -> Text -> Int -> SDB ()
rethreadCommentDB mnew_parent_id new_discussion_id root_comment_id user_id reason depth_offset = do
    (old_comment_ids, new_comment_ids) <- lift $ do
        descendants_ids <- fetchCommentAllCurrentDescendantsDB root_comment_id
        let old_comment_ids = root_comment_id : descendants_ids

        new_comment_ids <- flip State.evalStateT mempty $ forM old_comment_ids $ \comment_id -> do
            rethread_map <- State.get

            Just comment <- get comment_id

            let new_parent_id = maybe mnew_parent_id Just $ M.lookup (commentParent comment) rethread_map

            new_comment_id <- insert $ comment
                { commentDepth      = commentDepth comment - depth_offset
                , commentParent     = new_parent_id
                , commentDiscussion = new_discussion_id
                }

            State.put $ M.insert (Just comment_id) new_comment_id rethread_map

            return new_comment_id

        return (old_comment_ids, new_comment_ids)

    now <- liftIO getCurrentTime

    let new_root_comment_id = Prelude.head new_comment_ids -- This is kind of ugly, but it should be safe.
        rethread = Rethread now user_id root_comment_id new_root_comment_id reason
    rethread_id <- lift (insert rethread)
    tell [ECommentRethreaded rethread_id rethread]

    let updateForRethread :: (PersistEntity val, PersistEntityBackend val ~ SqlBackend)
                          => EntityField val CommentId
                          -> (SqlExpr (Entity val) -> SqlExpr (Entity CommentRethread) -> SqlExpr (Insertion val))
                          -> DB ()
        updateForRethread comment_field constructor =
            insertSelect $
            from $ \(table `InnerJoin` cr) -> do
            on_ (table ^. comment_field ==. cr ^. CommentRethreadOldComment)
            where_ (table ^. comment_field `in_` valList old_comment_ids)
            return (constructor table cr)

    lift $ do
        forM_ (zip old_comment_ids new_comment_ids) $ \(old_comment_id, new_comment_id) -> do
            insert_ (CommentRethread rethread_id old_comment_id new_comment_id)

            -- TODO(mitchell, david): pull the stuff below out of the for-loop

            insertSelect $
             from $ \(c `InnerJoin` ca) -> do
             on_ (c ^. CommentParent ==. just (ca ^. CommentAncestorComment))
             where_ (c ^. CommentId ==. val new_comment_id)
             return (CommentAncestor <# val new_comment_id <&> (ca ^. CommentAncestorAncestor))

            [Value maybe_new_parent_id] <-
                select $
                from $ \c -> do
                where_ (c ^. CommentId ==. val new_comment_id)
                return (c ^. CommentParent)

            maybe (return ()) (insert_ . CommentAncestor new_comment_id) maybe_new_parent_id

        -- EVERYTHING with a foreign key on CommentId needs to be added here, for the
        -- new comments. We don't want to update in-place because we *do* show the
        -- rethreaded comments on Project feeds (for one thing).

        updateForRethread CommentClosingComment
                          (\cc cr -> CommentClosing
                              <#  (cc  ^. CommentClosingTs)
                              <&> (cc  ^. CommentClosingClosedBy)
                              <&> (cc  ^. CommentClosingReason)
                              <&> (cr  ^. CommentRethreadNewComment))

        updateForRethread CommentFlaggingComment
                          (\cf cr -> CommentFlagging
                              <#  (cf  ^. CommentFlaggingTs)
                              <&> (cf  ^. CommentFlaggingFlagger)
                              <&> (cr  ^. CommentRethreadNewComment)
                              <&> (cf  ^. CommentFlaggingMessage))

        updateForRethread CommentRetractingComment
                          (\r cr -> CommentRetracting
                              <#  (r   ^. CommentRetractingTs)
                              <&> (r   ^. CommentRetractingReason)
                              <&> (cr  ^. CommentRethreadNewComment))

        updateForRethread CommentTagComment
                          (\ct cr -> CommentTag
                              <#  (cr  ^. CommentRethreadNewComment)
                              <&> (ct  ^. CommentTagTag)
                              <&> (ct  ^. CommentTagUser)
                              <&> (ct  ^. CommentTagCount))

        updateForRethread TicketComment
                          (\t cr -> Ticket
                              <#  (t   ^. TicketCreatedTs)
                              <&> (t   ^. TicketUpdatedTs)
                              <&> (t   ^. TicketName)
                              <&> (cr  ^. CommentRethreadNewComment))

        updateForRethread TicketClaimingTicket
                          (\tc cr -> TicketClaiming
                              <#  (tc  ^. TicketClaimingTs)
                              <&> (tc  ^. TicketClaimingUser)
                              <&> (cr  ^. CommentRethreadNewComment)
                              <&> (tc  ^. TicketClaimingNote))

        updateForRethread TicketOldClaimingTicket
                          (\toc cr -> TicketOldClaiming
                              <#  (toc  ^. TicketOldClaimingClaimTs)
                              <&> (toc  ^. TicketOldClaimingUser)
                              <&> (cr   ^. CommentRethreadNewComment)
                              <&> (toc  ^. TicketOldClaimingNote)
                              <&> (toc  ^. TicketOldClaimingReleaseNote)
                              <&> (toc  ^. TicketOldClaimingReleasedTs))

        updateForRethread UnapprovedCommentNotificationComment
                          (\ucn cr -> UnapprovedCommentNotification
                              <#  (cr  ^. CommentRethreadNewComment)
                              <&> (ucn ^. UnapprovedCommentNotificationNotification))

        updateForRethread ViewCommentComment
                          (\vc cr -> ViewComment
                              <#  (vc  ^. ViewCommentUser)
                              <&> (cr  ^. CommentRethreadNewComment))

fetchCommentTicketsDB :: Set CommentId -> DB (Map CommentId (Entity Ticket))
fetchCommentTicketsDB comment_ids = do
    ticket_entities <- select $ from $ \ t -> do
        where_ $ t ^. TicketComment `in_` valList (S.toList comment_ids)
        return t

    return $ M.fromList $ map (ticketComment . entityVal &&& id) ticket_entities

makeCommentRouteDB :: [Language] -> CommentId -> DB (Maybe (Route App))
makeCommentRouteDB langs comment_id = get comment_id >>= \case
    Nothing -> return Nothing
    Just comment -> fetchDiscussionDB langs (commentDiscussion comment) >>= \case
        DiscussionOnProject (Entity _ project) -> return $ Just $ ProjectCommentR (projectHandle project) comment_id

        DiscussionOnWikiPage (Entity _ wiki_target) -> do
            project <- getJust $ wikiTargetProject wiki_target
            return $ Just $ WikiCommentR (projectHandle project) (wikiTargetLanguage wiki_target) (wikiTargetTarget wiki_target) comment_id

        DiscussionOnUser (Entity user_id _) -> do
            return $ Just $ UserCommentR user_id comment_id

        DiscussionOnBlogPost (Entity _ blog_post) -> do
            project <- getJust $ blogPostProject blog_post

            return $ Just $ BlogPostCommentR (projectHandle project) (blogPostHandle blog_post) comment_id


