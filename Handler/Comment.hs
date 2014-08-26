-- | Handler functions that are shared among various different
-- locations Comments may exist.

module Handler.Comment
    -- Handlers
    ( deleteCommentDirectLinkR
    , getCommentDirectLinkR
    , getCommentTagR
    -- Utils
    , MakeCommentActionWidget
    , earlierClosuresFromMaybeParentId
    , getCommentTags
    , getMaxDepth
    , getMaxDepthDefault
    , getMaxDepthNoLimit
    , getProjectCommentAddTag
    , makeApproveCommentWidget
    , makeCloseCommentWidget
    , makeCommentForestWidget
    , makeCommentTreeWidget
    , makeDeleteCommentWidget
    , makeEditCommentWidget
    , makeFlagCommentWidget
    , makeReplyCommentWidget
    , makeRethreadCommentWidget
    , makeRetractCommentWidget
    , postCommentTag
    , postCommentApplyTag
    , postCommentCreateTag
    , postApproveComment
    , postCloseComment
    , postDeleteComment
    , postEditComment
    , postFlagComment
    , postNewComment
    , postRethreadComment
    , postRetractComment
    , redirectIfRethreaded
    ) where

import Import

import qualified Data.Tree.Extra                 as Tree
import           Handler.Utils
import           Model.Comment
import           Model.Comment.ActionPermissions
import           Model.Comment.HandlerInfo
import           Model.Comment.Routes
import           Model.Project
import           Model.Tag
import           Model.User
import           View.Comment
import           Widgets.Preview
import           Widgets.Tag

import           Data.Default                    (def)
import           Data.Tree                       (Forest, Tree, rootLabel)
import qualified Data.Map                        as M
import qualified Data.Set                        as S
import qualified Data.Text                       as T
import           Network.HTTP.Types.Status       (movedPermanently301)
import           Yesod.Default.Config            (appRoot)

--------------------------------------------------------------------------------
-- Utility functions

earlierClosuresFromMaybeParentId :: Maybe CommentId -> Handler [CommentClosure]
earlierClosuresFromMaybeParentId Nothing  = return []
earlierClosuresFromMaybeParentId (Just c) = runDB (fetchCommentAncestorClosuresDB' c)

-- | Get the max depth from the "maxdepth" GET param, or 11 (arbitrary) if it doesn't exist.
getMaxDepth :: Handler MaxDepth
getMaxDepth = getMaxDepthDefault 11

-- | Get the max depth from the "maxdepth" GET param, or NoMaxDepth if it doesn't exist.
getMaxDepthNoLimit :: Handler MaxDepth
getMaxDepthNoLimit = maybe NoMaxDepth MaxDepth <$> runInputGet (iopt intField "maxdepth")

-- | Get the max depth from the "maxdepth" GET param, or default to the provided depth.
getMaxDepthDefault :: Int -> Handler MaxDepth
getMaxDepthDefault n = maybe (MaxDepth n) MaxDepth <$> runInputGet (iopt intField "maxdepth")

redirectIfRethreaded :: CommentId -> Handler ()
redirectIfRethreaded comment_id = runDB (fetchCommentRethreadDB comment_id) >>= \case
    Nothing -> return ()
    Just new_comment_id -> redirectWith movedPermanently301 (CommentDirectLinkR new_comment_id)

-- | Make a Comment forest Widget. Also returns the comment forest directly, so that additional
-- actions may be taken on the comments (such as marking them all as viewed).
makeCommentForestWidget
        :: CommentHandlerInfo
        -> [Entity Comment]
        -> Maybe (Entity User)
        -> CommentMods                  -- ^ Comment structure modifications.
        -> Handler MaxDepth             -- ^ Max depth getter.
        -> Bool                         -- ^ Is this a preview?
        -> Widget                       -- ^ Widget to display under root comment.
        -> Handler (Widget, Forest (Entity Comment))
makeCommentForestWidget
        CommentHandlerInfo{..}
        roots
        mviewer
        CommentMods{..}
        get_max_depth
        is_preview
        form_under_root_comment = do
    let root_ids = map entityKey roots
    (children, user_map, earlier_closures_map, closure_map, ticket_map, flag_map) <- runDB $ do
        children <- fetchCommentsDescendantsDB root_ids commentHandlerHasPermission

        let all_comments    = roots ++ children
            all_comment_ids = map entityKey all_comments

        earlier_closures_map <- fetchCommentsAncestorClosuresDB root_ids
        user_map             <- entitiesMap <$> fetchUsersInDB (S.toList $ makeCommentUsersSet all_comments)
        closure_map          <- makeClosureMapDB all_comment_ids
        ticket_map           <- makeTicketMapDB  all_comment_ids
        flag_map             <- makeFlagMapDB    all_comment_ids

        return (children, user_map, earlier_closures_map, closure_map, ticket_map, flag_map)

    max_depth <- get_max_depth

    let user_map_with_viewer = (maybe id (onEntity M.insert) mviewer) user_map
        comment_forest = Tree.sortForestBy orderingNewestFirst (buildCommentForest roots children)
        comment_forest_widget =
            forM_ comment_forest $ \comment_tree -> do
                let root_id = entityKey (rootLabel comment_tree)
                    earlier_closures = M.findWithDefault [] root_id earlier_closures_map

                commentTreeWidget
                    comment_tree
                    (entityKey <$> mviewer)
                    commentHandlerRoutes
                    commentHandlerMakeActionPermissionsMap
                    (mod_earlier_closures earlier_closures)
                    (mod_user_map         user_map_with_viewer)
                    (mod_closure_map      closure_map)
                    (mod_ticket_map       ticket_map)
                    (mod_flag_map         flag_map)
                    is_preview
                    max_depth
                    0
                    form_under_root_comment

    return (comment_forest_widget, comment_forest)

-- | Make a Comment tree Widget. Also returns the comment tree directly, so that additional
-- actions may be taken on the comments (such as marking them all as viewed).
makeCommentTreeWidget
        :: CommentHandlerInfo
        -> Entity Comment               -- ^ Root comment.
        -> Maybe (Entity User)
        -> CommentMods                  -- ^ Comment structure modifications.
        -> Handler MaxDepth
        -> Bool                         -- ^ Is this a preview?
        -> Widget                       -- ^ Widget to display under root comment.
        -> Handler (Widget, Tree (Entity Comment))
makeCommentTreeWidget a b c d e f g  = do
    (widget, [tree]) <- makeCommentForestWidget a [b] c d e f g
    return (widget, tree)

type MakeCommentActionWidget
    = Entity Comment
   -> Entity User
   -> CommentHandlerInfo
   -> CommentMods
   -> Handler MaxDepth
   -> Bool -- is preview?
   -> Handler (Widget, Tree (Entity Comment))

-- | Make a comment action widget (close, delete, etc.). Unexported. Call one of
-- makeCloseCommentWidget, makeDeleteCommentWidget, etc. directly.
makeCommentActionWidget :: (CommentActionPermissions -> Bool) -> Widget -> MakeCommentActionWidget
makeCommentActionWidget
        can_perform_action
        form_widget
        comment@(Entity comment_id _)
        user
        handler_info
        mods
        get_max_depth
        is_preview = do
    action_permissions <-
        lookupErr "makeCommentActionWidget: comment id not found in map" comment_id
          <$> commentHandlerMakeActionPermissionsMap handler_info [comment]
    unless (can_perform_action action_permissions)
           (permissionDenied "You don't have permission to perform this action.")

    makeCommentTreeWidget
        handler_info
        comment
        (Just user)
        mods
        get_max_depth
        is_preview
        form_widget

makeApproveCommentWidget  :: MakeCommentActionWidget
makeCloseCommentWidget    :: MakeCommentActionWidget
makeEditCommentWidget     :: MakeCommentActionWidget
makeFlagCommentWidget     :: MakeCommentActionWidget
makeDeleteCommentWidget   :: MakeCommentActionWidget
makeReplyCommentWidget    :: MakeCommentActionWidget
makeRethreadCommentWidget :: MakeCommentActionWidget
makeRetractCommentWidget  :: MakeCommentActionWidget

makeApproveCommentWidget  = makeCommentActionWidget can_approve  approveCommentFormWidget
makeCloseCommentWidget    = makeCommentActionWidget can_close    (closeCommentFormWidget Nothing)
makeFlagCommentWidget     = makeCommentActionWidget can_flag     (flagCommentFormWidget Nothing Nothing)
makeDeleteCommentWidget   = makeCommentActionWidget can_delete   deleteCommentFormWidget
makeReplyCommentWidget    = makeCommentActionWidget can_reply    commentReplyFormWidget
makeRethreadCommentWidget = makeCommentActionWidget can_rethread rethreadCommentFormWidget
makeRetractCommentWidget  = makeCommentActionWidget can_retract  (retractCommentFormWidget Nothing)

makeEditCommentWidget
        comment
        user
        comment_handler_info
        mods
        get_max_depth
        is_preview = do
    makeCommentActionWidget
      can_edit
      (editCommentFormWidget (commentText (entityVal comment)))
      comment
      user
      comment_handler_info
      mods
      get_max_depth
      is_preview

-- | Handle a GET to a /tag/new URL on a Project's Comment. This is distinct from
-- adding a tag on some other Comment, because we want to display the Project's
-- existing tags. Permission checking should occur *PRIOR TO* this function.
getProjectCommentAddTag :: CommentId -> ProjectId -> UserId -> Handler Html
getProjectCommentAddTag comment_id project_id user_id = do
    (tag_map, tags, project_tags, other_tags) <- runDB $ do
        comment_tags <- fetchCommentCommentTagsDB comment_id
        tag_map      <- entitiesMap <$> fetchTagsInDB (map commentTagTag comment_tags)
        tags         <- M.findWithDefault [] comment_id <$> buildAnnotatedCommentTagsDB (Just user_id) comment_tags
        (project_tags, other_tags) <- getProjectTagList project_id
        return (tag_map, tags, project_tags, other_tags)

    let filter_tags = filter (\(Entity t _) -> not $ M.member t tag_map)
    (apply_form, _)  <- generateFormPost $ newCommentTagForm (filter_tags project_tags) (filter_tags other_tags)
    (create_form, _) <- generateFormPost $ createCommentTagForm

    defaultLayout $(widgetFile "new_comment_tag")

-- | Handle a POST to a /moderate URL. Permission checking should occur *PRIOR TO* this function.
postApproveComment :: UserId -> CommentId -> Comment -> Handler ()
postApproveComment user_id comment_id comment = do
    runSDB (approveCommentDB user_id comment_id comment)
    alertSuccess "comment approved"

postCloseComment, postRetractComment :: Entity User -> CommentId -> Comment -> CommentHandlerInfo -> Handler (Maybe (Widget, Widget))
postCloseComment   = postClosureComment closeCommentForm   newClosedCommentClosure    can_close
postRetractComment = postClosureComment retractCommentForm newRetractedCommentClosure can_retract

-- | Handle a POST to a /close or /retract URL.
-- Permission checking should occur *PRIOR TO* this function.
postClosureComment
        :: (Maybe Markdown -> Form Markdown)
        -> (UserId -> Markdown -> CommentId -> Handler CommentClosure)
        -> (CommentActionPermissions -> Bool)
        -> Entity User
        -> CommentId
        -> Comment
        -> CommentHandlerInfo
        -> Handler (Maybe (Widget, Widget))
postClosureComment
        make_closure_form
        make_new_comment_closure
        can_perform_action
        user@(Entity user_id _)
        comment_id
        comment
        comment_handler_info = do
    ((result, _), _) <- runFormPost (make_closure_form Nothing)
    case result of
        FormSuccess reason -> do
            new_comment_closure <- make_new_comment_closure user_id reason comment_id
            lookupPostMode >>= \case
                Just PostMode -> do
                    runDB (insert_ new_comment_closure)
                    return Nothing
                _ -> do
                    (form, _) <- generateFormPost (make_closure_form (Just reason))
                    (comment_widget, _) <-
                        makeCommentActionWidget
                          can_perform_action
                          mempty
                          (Entity comment_id comment)
                          user
                          comment_handler_info
                          (def { mod_closure_map = M.insert comment_id new_comment_closure })
                          (getMaxDepthDefault 0)
                          True

                    return (Just (comment_widget, form))
        _ -> error "Error when submitting form."

-- | Handle a POST to a /delete URL. Returns whether or not the Comment was deleted,
-- per the "mode" POST param (True = deleted, False = not deleted).
-- Permission checking should occur *PRIOR TO* this function.
postDeleteComment :: CommentId -> Handler Bool
postDeleteComment comment_id =
    lookupPostMode >>= \case
        Just PostMode -> do
            deleteCommentDirectLinkR comment_id
            alertSuccess "comment deleted"
            return True
        _ -> return False

-- | Handle a POST to an /edit URL. Returns Nothing if the comment was edited, or Just Widget
-- if there's a preview widget to display (per POST param "mode").
-- Permission checking should occur *PRIOR TO* this function.
postEditComment :: Entity User -> Entity Comment -> CommentHandlerInfo -> Handler (Maybe Widget)
postEditComment user comment@(Entity comment_id _) comment_handler_info = do
    ((result, _), _) <- runFormPost (editCommentForm "")
    case result of
        FormSuccess new_text -> lookupPostMode >>= \case
            Just PostMode -> do
                runSYDB (editCommentDB comment_id new_text)
                alertSuccess "posted new edit"
                return Nothing
            _ -> do
                (form, _) <- generateFormPost (editCommentForm new_text)
                (comment_widget, _) <-
                    makeCommentActionWidget
                      can_edit
                      mempty
                      comment
                      user
                      comment_handler_info
                      (def { mod_comment = \c -> c { commentText = new_text }
                           -- Since an edit removes a flagging, don't show the flagged markup in preview.
                           , mod_flag_map = M.delete comment_id
                           })
                      (getMaxDepthDefault 0)
                      True
                return (Just (previewWidget form "post" comment_widget))
        FormMissing -> error "Form missing."
        FormFailure msgs -> error $ "Error submitting form: " ++ T.unpack (T.intercalate "\n" msgs)

-- | Handle a POST to a /flag URL.
-- Permission checking should occur *PRIOR TO* this function.
postFlagComment :: Entity User -> Entity Comment -> CommentHandlerInfo -> Handler (Maybe Widget)
postFlagComment user@(Entity user_id _) comment@(Entity comment_id _) comment_handler_info = do
    ((result, _), _) <- runFormPost (flagCommentForm Nothing Nothing)
    case result of
        -- TODO(mitchell): Change the form to just return [FlagReason], not Maybe [FlagReason]
        FormSuccess (Nothing, _) -> flagFailure "Please check at least one Code of Conduct violation."
        FormSuccess (Just [], _) -> flagFailure "Please check at least one Code of Conduct violation."
        FormSuccess (Just reasons, message) -> lookupPostMode >>= \case
            Just PostMode -> do
                let permalink_route = comment_route_edit (commentHandlerRoutes comment_handler_info) comment_id
                permalink_route_text <- getUrlRender <*> pure permalink_route
                success <- runSYDB (flagCommentDB comment_id permalink_route_text user_id reasons message)
                if success
                    then alertSuccess "comment hidden and flagged for revision"
                    else alertDanger "error: another user flagged this just before you"
                return Nothing
            _ -> do
                (form, _) <- generateFormPost $ flagCommentForm (Just (Just reasons)) (Just message)

                let style_widget =
                        -- the CSS below styles this particular flagging submit
                        -- button. It would be ideal to have this in a more
                        -- generalized place so it can be reused in other flagging
                        -- buttons and be in just one place, but this works for
                        -- now.
                        toWidget [cassius|
                            .preview-action-button[type=submit]
                                background : dark-red
                                background-image : linear-gradient(#ee2700, #bd1000)
                                border-color: #a5022a

                            .preview-action-button[type=submit]:hover, .preview-action-button[type=submit]:focus, .preview-action-button[type=submit]:active
                                background : red
                                background-image : linear-gradient(#d22935, #a5022a)
                        |]
                    form_with_header =
                        [whamlet|
                            <h4>Code of Conduct Violation(s):
                            ^{form}
                        |]

                (comment_widget, _) <-
                    makeCommentActionWidget
                      can_flag
                      mempty
                      comment
                      user
                      comment_handler_info
                      (def { mod_flag_map = M.insert comment_id (message, reasons) })
                      (getMaxDepthDefault 0)
                      True
                return (Just (style_widget <> previewWidget form_with_header "flag comment" comment_widget))

        FormFailure errs -> flagFailure (T.intercalate ", " errs)
        _ -> flagFailure "Form missing."
  where
    flagFailure :: Text -> Handler a
    flagFailure msg = do
        alertDanger msg
        Just route <- getCurrentRoute
        redirect route

-- | Handle a POST to either a /reply or /d URL (reply, or new topic). Checks the POST params for
-- "mode" key - could either be a "post" (posts the comment and returns its id) or a "preview"
-- (returns the comment tree and form, to wrap in a preview). Permission checking should occur
-- *PRIOR TO* this function.
postNewComment :: Maybe CommentId -> Entity User -> DiscussionId -> MakeActionPermissionsMap -> Handler (Either CommentId (Widget, Widget))
postNewComment mparent_id (Entity user_id user) discussion_id make_permissions_map = do
    -- commentReplyForm is OK here (the alternative is commentNewTopicForm) because they're
    -- actually the same form with different titles.
    ((result, _), _) <- runFormPost commentReplyForm
    case result of
        FormSuccess contents -> lookupPostMode >>= \case
            Just PostMode -> do
                if userIsEstablished user
                    then do
                        comment_id <- runSDB (postApprovedCommentDB user_id mparent_id discussion_id contents)
                        alertSuccess "comment posted"
                        return (Left comment_id)
                    else do
                        comment_id <- runSDB (postUnapprovedCommentDB user_id mparent_id discussion_id contents)
                        alertSuccess "comment submitted for moderation"
                        return (Left comment_id)
            _ -> do
                earlier_closures    <- earlierClosuresFromMaybeParentId mparent_id
                depth               <- runDB (fetchCommentDepthFromMaybeParentIdDB mparent_id)
                (form, _)           <- generateFormPost (commentForm (maybe "New Topic" (const "Reply") mparent_id) (Just contents))
                now                 <- liftIO getCurrentTime

                let (approved_ts, approved_by) = if userIsEstablished user
                                                       then (Just now, Just user_id)
                                                       else (Nothing, Nothing)
                    comment = Entity
                                (Key $ PersistInt64 0)
                                (Comment now approved_ts approved_by discussion_id mparent_id user_id contents depth)

                max_depth <- getMaxDepthDefault 0

                let comment_tree =
                        commentTreeWidget
                          (Tree.singleton comment)
                          (Just user_id)
                          dummyCommentRoutes -- 'True' below, so routes aren't used.
                          make_permissions_map
                          earlier_closures
                          (M.singleton user_id user)
                          mempty -- closure map
                          mempty -- ticket map - TODO(mitchell): this isn't right... if *this* comment is a ticket, we should display it as such.
                          mempty -- flag map
                          True
                          max_depth
                          0
                          mempty

                return (Right (comment_tree, form))
        FormMissing -> error "Form missing."
        FormFailure msgs -> error $ "Error submitting form: " ++ T.unpack (T.intercalate "\n" msgs)

postRethreadComment :: UserId -> CommentId -> Comment -> Handler Html
postRethreadComment user_id comment_id comment = do
    -- TODO(david): AVOID CYCLES

    ((result, _), _) <- runFormPost rethreadCommentForm
    case result of
        FormSuccess (new_parent_url, reason) -> do
            app <- getYesod
            let splitPath  = drop 1 . T.splitOn "/"
                stripQuery = fst . T.break (== '?')
                stripRoot  = fromMaybe new_parent_url . T.stripPrefix (appRoot $ settings app)
                url        = splitPath $ stripQuery (stripRoot new_parent_url)

            let notfound = error "could not find discussion for that URL"

            -- FIXME(mitchell,david): We shouldn't have to enumerate the routes like this.
            -- Luckily robust rethreading is not priority.
            (new_parent_id, new_discussion_id) <- case parseRoute (url, []) of
                Just (WikiCommentR new_project_handle new_target new_parent_id) -> do
                    new_discussion_id <-
                        maybe notfound (wikiPageDiscussion . entityVal) <$>
                          runDB (fetchProjectWikiPageByNameDB new_project_handle new_target)
                    return (Just new_parent_id, new_discussion_id)
                Just (WikiDiscussionR new_project_handle new_target) -> do
                    new_discussion_id <-
                        maybe notfound (wikiPageDiscussion . entityVal) <$>
                          runDB (fetchProjectWikiPageByNameDB new_project_handle new_target)
                    return (Nothing, new_discussion_id)
                Just (ProjectCommentR new_project_handle new_parent_id) -> do
                    new_discussion_id <-
                        maybe notfound (projectDiscussion . entityVal) <$>
                          runDB (getBy (UniqueProjectHandle new_project_handle))
                    return (Just new_parent_id, new_discussion_id)
                Just (ProjectDiscussionR new_project_handle) -> do
                    new_discussion_id <-
                        maybe notfound (projectDiscussion . entityVal) <$>
                          runDB (getBy (UniqueProjectHandle new_project_handle))
                    return (Nothing, new_discussion_id)

                Nothing -> error "failed to parse URL"
                _       -> notfound

            let old_parent_id = commentParent comment
            when (new_parent_id == old_parent_id && new_discussion_id == commentDiscussion comment) $
                error "trying to move comment to its current location"

            new_parent_depth <- maybe (return $ -1) fetchCommentDepth404DB new_parent_id
            old_parent_depth <- maybe (return $ -1) fetchCommentDepth404DB old_parent_id

            let depth_offset = old_parent_depth - new_parent_depth

            lookupPostMode >>= \case
                Just PostMode -> do
                    now <- liftIO getCurrentTime

                    runDB $ do
                        descendants <- fetchCommentAllDescendantsDB comment_id
                        rethread_id <- insert (Rethread now user_id comment_id reason)
                        let comments = comment_id : descendants
                        new_comment_ids <- rethreadCommentsDB rethread_id depth_offset new_parent_id new_discussion_id comments

                        delete $
                         from $ \ca ->
                         where_ $ ca ^. CommentAncestorComment `in_` valList comments

                        forM_ new_comment_ids $ \ new_comment_id -> do
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

                        when (new_discussion_id /= commentDiscussion comment) $
                            update $ \c -> do
                            set c [ CommentDiscussion =. val new_discussion_id ]
                            where_ (c ^. CommentId `in_` valList descendants)

                    redirect new_parent_url

                _ -> error "no preview for rethreads yet" -- TODO(david)
        _ -> error "Error when submitting form."

getCommentTags :: CommentId -> Handler Html
getCommentTags comment_id = do
    muser_id <- maybeAuthId
    tags <- runDB $ M.findWithDefault [] comment_id <$> (fetchCommentCommentTagsDB comment_id >>= buildAnnotatedCommentTagsDB muser_id)
    defaultLayout $(widgetFile "tags")

postCommentTag :: CommentId -> TagId -> Handler ()
postCommentTag comment_id tag_id = do
    user_id <- requireAuthId
    direction <- lookupPostParam "direction"

    let delta = case T.unpack <$> direction of
            Just "+" -> 1
            Just "-" -> -1
            Just "\215" -> -1
            Nothing -> error "direction unset"
            Just str -> error $ "unrecognized direction: " ++ str

    runDB $ do
        maybe_comment_tag_entity <- getBy (UniqueCommentTag comment_id tag_id user_id)
        case maybe_comment_tag_entity of
            Nothing -> insert_ (CommentTag comment_id tag_id user_id delta)
            Just (Entity comment_tag_id comment_tag) -> case commentTagCount comment_tag + delta of
                0 -> delete $ from $ \ ct -> where_ $ ct ^. CommentTagId ==. val comment_tag_id
                x -> void $ update $ \ ct -> do
                    set ct [ CommentTagCount =. val x ]
                    where_ $ ct ^. CommentTagId ==. val comment_tag_id

postCommentApplyTag :: CommentId -> Handler ()
postCommentApplyTag comment_id = do
    Entity user_id user <- requireAuth

    unless (userCanAddTag user) $
        permissionDenied "You must be an established user to add tags"

    ((result_apply, _), _) <- runFormPost (newCommentTagForm [] [])
    case result_apply of
        FormSuccess (mproject_tag_ids, mother_tag_ids) -> do
            let project_tag_ids = fromMaybe [] mproject_tag_ids
                other_tag_ids   = fromMaybe [] mother_tag_ids

            ok <- runDB $ do
                valid_tags <- fetchTagsInDB (project_tag_ids <> other_tag_ids)
                if null valid_tags
                    then return False
                    else do
                        void (insertMany $ fmap (\(Entity tag_id _) -> CommentTag comment_id tag_id user_id 1) valid_tags)
                        return True
            unless ok (permissionDenied "Error: Invalid tag ID.")
        FormMissing -> error "form missing"
        FormFailure errs -> error $ T.unpack $ "Form failed: " <> T.intercalate "; " errs

postCommentCreateTag :: CommentId -> Handler ()
postCommentCreateTag comment_id = do
    Entity user_id user <- requireAuth

    unless (userCanAddTag user) $
        permissionDenied "You must be an established user to add tags"

    ((result_create, _), _) <- runFormPost $ createCommentTagForm
    case result_create of
        FormSuccess tag_name -> do
            tag_exists <- runDB $ getBy (UniqueTag tag_name) >>= \case
                Nothing -> do
                    tag_id <- insert $ Tag tag_name
                    insert_ (CommentTag comment_id tag_id user_id 1)
                    return False
                Just _ -> return True
            when tag_exists (alertDanger "That tag already exists.")
        FormMissing -> error "form missing"
        FormFailure errs -> error $ T.unpack $ "Form failed: " <> T.intercalate "; " errs

--------------------------------------------------------------------------------
-- /

getCommentDirectLinkR :: CommentId -> Handler Html
getCommentDirectLinkR comment_id = runDB (fetchCommentWikiPageDB comment_id) >>= \case
    -- comment not on a wiki page? right now, there's nowhere else to check
    -- TODO(mitchell): does this require constant attention?
    Nothing -> notFound
    Just (Entity _ page) -> do
        project <- runYDB (get404 (wikiPageProject page))
        redirect (WikiCommentR (projectHandle project) (wikiPageTarget page) comment_id)

deleteCommentDirectLinkR :: CommentId -> Handler ()
deleteCommentDirectLinkR comment_id = do
    user_id <- requireAuthId
    comment <- runYDB (get404 comment_id)

    ok <- runDB $ do
        can_delete <- userCanDeleteCommentDB user_id (Entity comment_id comment)
        if can_delete
            then deleteCommentDB comment_id >> return True
            else return False
    unless ok (permissionDenied "You don't have permission to delete that comment.")

--------------------------------------------------------------------------------
-- /c/

getCommentTagR :: CommentId -> TagId -> Handler Html
getCommentTagR comment_id tag_id = do
    muser_id <- maybeAuthId
    tags <- runDB $ M.findWithDefault [] comment_id <$> (fetchCommentTagCommentTagsDB comment_id tag_id >>= buildAnnotatedCommentTagsDB muser_id)
    case tags of
        [] -> error "That tag has not been applied to this comment."
        [tag] -> renderTag tag
        _ -> error "This should never happen."
  where
    renderTag (AnnotatedTag tag _ _ user_votes) = do
        let tag_name = tagName $ entityVal tag
        defaultLayout $(widgetFile "tag")

