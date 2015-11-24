-- | Handler functions that are shared among various different
-- locations Comments may exist.

module Handler.Comment
    -- Handlers
    ( deleteCommentDirectLinkR
    , getCommentDirectLinkR
    , getCommentTagR
    , postCommentTagR

    -- Utils
    , MakeCommentActionWidget
    , CommentPostResult(..)
    , getCommentTags
    , getMaxDepth
    , getMaxDepthDefault
    , getMaxDepthNoLimit
    , getProjectCommentAddTag
    , getUserCommentAddTag
    , makeApproveCommentWidget
    , makeClaimCommentWidget
    , makeCloseCommentWidget
    , makeCommentForestWidget
    , makeCommentTreeWidget
    , makeDeleteCommentWidget
    , makeEditCommentWidget
    , makeFlagCommentWidget
    , makeReplyCommentWidget
    , makeRethreadCommentWidget
    , makeRetractCommentWidget
    , makeUnclaimCommentWidget
    , makeWatchCommentWidget
    , makeUnwatchCommentWidget
    , postCommentApplyTag
    , postCommentCreateTag
    , postApproveComment
    , postClaimComment
    , postCloseComment
    , postDeleteComment
    , postEditComment
    , postFlagComment
    , postNewComment
    , postRethreadComment
    , postRetractComment
    , postUnclaimComment
    , postWatchComment
    , postUnwatchComment
    , redirectIfRethreaded
    ) where

import Import

import Control.Monad.Writer (tell)
import Data.Default (def)
import Data.Tree (Forest, Tree, rootLabel)
import Network.HTTP.Types.Status (movedPermanently301)
import Yesod.Default.Config (appRoot)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Tree.Extra as Tree

import Handler.Utils
import Model.Comment
import Model.Comment.ActionPermissions
import Model.Comment.HandlerInfo
import Model.Comment.Mods
import Model.Comment.Routes
import Model.Project
import Model.Tag
import Model.User
import Model.Utils
import View.Comment
import Widgets.Tag

--------------------------------------------------------------------------------
-- Utility functions



-- | Get the max depth from the "maxdepth" GET param, or 11 (arbitrary) if
-- it doesn't exist.
getMaxDepth :: Handler MaxDepth
getMaxDepth = getMaxDepthDefault 11

-- | Get the max depth from the "maxdepth" GET param, or NoMaxDepth if it
-- doesn't exist.
getMaxDepthNoLimit :: Handler MaxDepth
getMaxDepthNoLimit =
    fmap (maybe NoMaxDepth MaxDepth)
         (runInputGet (iopt intField "maxdepth"))

-- | Get the max depth from the "maxdepth" GET param, or default to the
-- provided depth.
getMaxDepthDefault :: Int -> Handler MaxDepth
getMaxDepthDefault n =
    fmap (MaxDepth . fromMaybe n)
         (runInputGet (iopt intField "maxdepth"))

redirectIfRethreaded :: CommentId -> Handler ()
redirectIfRethreaded = go <=< (runYDB . fetchCommentRethreadDB)
  where
    go = \case
        Nothing -> return ()
        Just new_comment_id ->
            redirectWith movedPermanently301 (CommentDirectLinkR new_comment_id)

-- | Make a Comment forest Widget.
-- Also returns the comment forest directly, so further actions may be taken
-- (such as marking all comments as viewed).
makeCommentForestWidget
        :: (CommentMods -> CommentHandlerInfo)
        -> [Entity Comment]
        -> Maybe (Entity User)
        -> CommentMods              -- ^ Comment structure modifications.
        -> MaxDepth                 -- ^ Max depth
        -> Bool                     -- ^ Is this a preview?
        -> Widget                   -- ^ Widget to display under root comment.
        -> Handler (Widget, Forest (Entity Comment))
makeCommentForestWidget
        make_comment_handler_info
        roots
        mviewer
        mods@CommentMods{..}
        max_depth
        is_preview
        form_under_root_comment = do
    let CommentHandlerInfo{..} = make_comment_handler_info mods

    CommentForestData{..} <-
        runDB $ fetchCommentForestData roots commentHandlerHasPermission

    let user_map_with_viewer = maybe id (onEntity M.insert) mviewer user_map
        comment_forest =
            Tree.sortForestBy
                orderingNewestFirst (buildCommentForest roots children)
        comment_forest_widget =
            forM_ comment_forest $ \comment_tree -> do
                let root_id = entityKey (rootLabel comment_tree)
                    earlier_closures =
                        M.findWithDefault [] root_id earlier_closures_map
                    earlier_retracts =
                        M.findWithDefault [] root_id earlier_retracts_map

                commentTreeWidget
                    comment_tree
                    (entityKey <$> mviewer)
                    commentHandlerRoutes
                    commentHandlerMakeActionPermissionsMap
                    (mod_earlier_closures earlier_closures)
                    (mod_earlier_retracts earlier_retracts)
                    (mod_user_map         user_map_with_viewer)
                    (mod_closure_map      closure_map)
                    (mod_retract_map      retract_map)
                    (mod_ticket_map       ticket_map)
                    (mod_claim_map        claim_map)
                    (mod_flag_map         flag_map)
                    is_preview
                    max_depth
                    0
                    form_under_root_comment

    return (comment_forest_widget, comment_forest)

-- | Make a Comment tree Widget.
-- Also returns the tree directly, so that additional actions may be taken
-- (such as marking all comments as viewed).
makeCommentTreeWidget
        :: (CommentMods -> CommentHandlerInfo)
        -> Entity Comment           -- ^ Root comment.
        -> Maybe (Entity User)
        -> CommentMods              -- ^ Comment structure modifications.
        -> MaxDepth
        -> Bool                     -- ^ Is this a preview?
        -> Widget                   -- ^ Widget to display under root comment.
        -> Handler (Widget, Tree (Entity Comment))
makeCommentTreeWidget a b c d e f g  = do
    (widget, [tree]) <- makeCommentForestWidget a [b] c d e f g
    return (widget, tree)

type MakeCommentActionWidget
    = Entity Comment
   -> Entity User
   -> (CommentMods -> CommentHandlerInfo)
   -> CommentMods
   -> MaxDepth
   -> Bool -- is preview?
   -> Handler (Widget, Tree (Entity Comment))

-- | Make a comment action widget (close, delete, etc.). Unexported. Call
-- makeCloseCommentWidget, makeDeleteCommentWidget, etc. directly.
makeCommentActionWidget
    :: (CommentActionPermissions -> Bool)
    -> Widget
    -> MakeCommentActionWidget
makeCommentActionWidget
        can_perform_action
        form_widget
        comment@(Entity comment_id _)
        user
        make_handler_info
        mods
        max_depth
        is_preview = do
    -- Just checking action permissions - we *don't* want to pass
    -- make_handler_info 'mods'. Consider previewing a 'close' action:
    -- presumably, 'mods' will add the comment to the 'close' map, which
    -- would mean the 'can_close' action would be False!
    action_permissions <-
        lookupErr lookupMsg comment_id
          <$> commentHandlerMakeActionPermissionsMap
                  (make_handler_info def)
                  [comment]
    unless (can_perform_action action_permissions)
           (permissionDenied deniedMsg)

    makeCommentTreeWidget
        make_handler_info
        comment
        (Just user)
        mods
        max_depth
        is_preview
        form_widget
  where
    lookupMsg = "makeCommentActionWidget: comment id not found in map"
    deniedMsg = "You don't have permission to perform this action."

makeApproveCommentWidget  :: MakeCommentActionWidget
makeClaimCommentWidget    :: MakeCommentActionWidget
makeCloseCommentWidget    :: MakeCommentActionWidget
makeEditCommentWidget     :: MakeCommentActionWidget
makeFlagCommentWidget     :: MakeCommentActionWidget
makeDeleteCommentWidget   :: MakeCommentActionWidget
makeReplyCommentWidget    :: MakeCommentActionWidget
makeRethreadCommentWidget :: MakeCommentActionWidget
makeRetractCommentWidget  :: MakeCommentActionWidget
makeUnclaimCommentWidget  :: MakeCommentActionWidget
makeWatchCommentWidget    :: MakeCommentActionWidget
makeUnwatchCommentWidget  :: MakeCommentActionWidget

makeApproveCommentWidget =
     makeCommentActionWidget can_approve approveCommentFormWidget
makeClaimCommentWidget =
     makeCommentActionWidget can_claim (claimCommentFormWidget Nothing)
makeCloseCommentWidget =
     makeCommentActionWidget can_close (closeCommentFormWidget Nothing)
makeFlagCommentWidget =
     makeCommentActionWidget can_flag (flagCommentFormWidget Nothing Nothing)
makeDeleteCommentWidget =
     makeCommentActionWidget can_delete deleteCommentFormWidget
makeReplyCommentWidget =
     makeCommentActionWidget can_reply commentReplyFormWidget
makeRethreadCommentWidget =
     makeCommentActionWidget can_rethread rethreadCommentFormWidget
makeRetractCommentWidget =
     makeCommentActionWidget can_retract (retractCommentFormWidget Nothing)
makeUnclaimCommentWidget =
     makeCommentActionWidget can_unclaim (unclaimCommentFormWidget Nothing)
makeWatchCommentWidget =
     makeCommentActionWidget can_watch watchCommentFormWidget
makeUnwatchCommentWidget =
     makeCommentActionWidget can_unwatch unwatchCommentFormWidget

makeEditCommentWidget
        comment
        user
        make_comment_handler_info
        mods
        get_max_depth
        is_preview = do
    let commentVal = entityVal comment
    makeCommentActionWidget
      can_edit
      (editCommentFormWidget (commentText commentVal)
                             (commentLanguage commentVal))
      comment
      user
      make_comment_handler_info
      mods
      get_max_depth
      is_preview

-- | Handle a GET to a /tag/new URL on a Project's Comment. This is
-- distinct from adding a tag on some other Comment, because we want to
-- display the Project's existing tags. Permission checking should occur
-- *PRIOR TO* this function.
getProjectCommentAddTag :: CommentId -> ProjectId -> UserId -> Handler Html
getProjectCommentAddTag comment_id project_id user_id = do
    (tag_map, tags, project_tags, other_tags) <- runDB $ do
        comment_tags <- fetchCommentCommentTagsDB comment_id
        tag_map <-
            entitiesMap <$> fetchTagsInDB (map commentTagTag comment_tags)
        tags <-
            M.findWithDefault [] comment_id <$>
                buildAnnotatedCommentTagsDB (Just user_id) comment_tags
        (project_tags, other_tags) <- getProjectTagList project_id
        return (tag_map, tags, project_tags, other_tags)

    let filter_tags = filter (\(Entity t _) -> not $ M.member t tag_map)
    (apply_form, _)  <-
        generateFormPost
            (newCommentTagForm
                (filter_tags project_tags)
                (filter_tags other_tags))
    (create_form, _) <- generateFormPost createCommentTagForm

    defaultLayout $(widgetFile "new_comment_tag")

-- | Handle a GET to a /tag/new URL on a User discussion's Comment.
getUserCommentAddTag :: CommentId -> UserId -> Handler Html
getUserCommentAddTag comment_id viewer_id = do
    (tag_map, tags, all_tags) <- runDB $ do
        comment_tags <- fetchCommentCommentTagsDB comment_id
        tag_map <-
            entitiesMap <$> fetchTagsInDB (map commentTagTag comment_tags)
        tags <-
            M.findWithDefault [] comment_id <$>
                buildAnnotatedCommentTagsDB (Just viewer_id) comment_tags
        all_tags <- select $ from return
        return (tag_map, tags, all_tags)

    let filter_tags = filter (\(Entity t _) -> not $ M.member t tag_map)
    (apply_form, _) <-
        generateFormPost $ newCommentTagForm [] (filter_tags all_tags)
    (create_form, _) <- generateFormPost createCommentTagForm

    defaultLayout $(widgetFile "new_comment_tag")

-- | Handle a POST to a /approve URL.
-- Permission checking should occur *PRIOR TO* this function.
postApproveComment :: UserId -> CommentId -> Comment -> Handler ()
postApproveComment user_id comment_id comment = do
    runSDB (approveCommentDB user_id comment_id comment)
    alertSuccess "comment approved"

postClaimComment
    :: Entity User
    -> CommentId
    -> Comment
    -> (CommentMods -> CommentHandlerInfo)
    -> Handler (Maybe (Widget, Widget))
postClaimComment
        user@(Entity user_id _)
        comment_id
        comment
        make_comment_handler_info = do
    ((result, _), _) <- runFormPost (claimCommentForm Nothing)
    maxDepth <- getMaxDepthDefault 0
    case result of
        FormSuccess mnote ->
            lookupPostMode >>= \case
                Just PostMode -> do
                    runSDB (userClaimCommentDB user_id comment_id mnote)
                    return Nothing
                _ -> do
                    now <- liftIO getCurrentTime
                    (form, _) <-
                        generateFormPost (claimCommentForm (Just mnote))
                    (comment_widget, _) <-
                        makeCommentActionWidget
                            can_claim
                            mempty
                            (Entity comment_id comment)
                            user
                            make_comment_handler_info
                            (mods now mnote)
                            maxDepth
                            True
                    return (Just (comment_widget, form))
        _ -> error "Error when submitting form."
  where
    mods now mnote = def {
        mod_claim_map =
            M.insert comment_id
                     (TicketClaiming now user_id comment_id mnote)}

-- | Handle a POST to a /close URL.
-- Permission checking should occur *PRIOR TO* this function.
postCloseComment
        :: Entity User
        -> CommentId
        -> Comment
        -> (CommentMods -> CommentHandlerInfo)
        -> Handler (Maybe (Widget, Widget))
postCloseComment
    user@(Entity user_id _)
    comment_id
    comment
    make_comment_handler_info = do
    ((result, _), _) <- runFormPost (closeCommentForm Nothing)
    maxDepth <- getMaxDepthDefault 0
    case result of
        FormSuccess (NewClosure reason) -> do
            now <- liftIO getCurrentTime
            let closing = CommentClosing now user_id reason comment_id
            lookupPostMode >>= \case
                Just PostMode -> do
                    runSDB $ do
                        closing_id <- lift $ insert closing
                        tell [ECommentClosed closing_id closing]

                    return Nothing
                _ -> do
                    (form, _) <-
                        generateFormPost (closeCommentForm (Just reason))
                    (comment_widget, _) <-
                        makeCommentActionWidget
                          can_close
                          mempty
                          (Entity comment_id comment)
                          user
                          make_comment_handler_info
                          (mods closing)
                          maxDepth
                          True

                    return (Just (comment_widget, form))
        _ -> error "Error when submitting form."
  where
    mods closing = def { mod_closure_map = M.insert comment_id closing }

-- | Handle a POST to a /delete URL.
-- Returns whether the Comment was deleted, per the "mode" POST param.
-- (True = deleted, False = not deleted).
-- Permission checking should occur *PRIOR TO* this function.
postDeleteComment :: CommentId -> Handler Bool
postDeleteComment comment_id =
    lookupPostMode >>= \case
        Just PostMode -> do
            deleteCommentDirectLinkR comment_id
            alertSuccess "comment deleted"
            return True
        _ -> return False

-- | Handle a POST to an /edit URL.
-- Returns Nothing if the comment was edited.
-- Returns Just Widget to display a preview widget per the "mode" POST param.
-- Permission checking should occur *PRIOR TO* this function.
postEditComment
        :: Entity User
        -> Entity Comment
        -> (CommentMods -> CommentHandlerInfo)
        -> Handler (Maybe (Widget, Widget))
postEditComment
        user@(Entity user_id _)
        (Entity comment_id comment)
        make_comment_handler_info = do
    ((result, _), _) <-
        runFormPost (editCommentForm "" (commentLanguage comment))
    maxDepth <- getMaxDepthDefault 0
    case result of
        FormSuccess (EditComment new_text new_language) -> lookupPostMode >>= \case
            Just PostMode -> do
                runSYDB (editCommentDB user_id comment_id new_text new_language)
                    >>= \case
                        Left err -> alertDanger err
                        Right () -> alertSuccess "posted new edit"
                return Nothing
            _ -> do
                (form, _) <- generateFormPost (editCommentForm new_text new_language)
                (comment_widget, _) <-
                    makeCommentActionWidget
                        can_edit
                        mempty
                        (Entity comment_id (comment { commentText = new_text }))
                        user
                        make_comment_handler_info
                        -- Since an edit removes a flagging, don't show the flagged markup in preview.
                        (def { mod_flag_map = M.delete comment_id })
                        maxDepth
                        True

                return (Just (comment_widget, form))

        FormMissing -> error "Form missing."
        FormFailure msgs -> error $ "Error submitting form: " ++ T.unpack (T.intercalate "\n" msgs)

-- | Handle a POST to a /flag URL.
-- Permission checking should occur *PRIOR TO* this function.
postFlagComment :: Entity User -> Entity Comment -> (CommentMods -> CommentHandlerInfo) -> Handler (Maybe (Widget, Widget))
postFlagComment user@(Entity user_id _) comment@(Entity comment_id _) make_comment_handler_info = do
    maxDepth <- getMaxDepthDefault 0
    ((result, _), _) <- runFormPost (flagCommentForm Nothing Nothing)
    case result of
        -- TODO: Change the form to just return [FlagReason], not Maybe [FlagReason]
        FormSuccess (Nothing, _) -> flagFailure "Please check at least one Code of Conduct violation."
        FormSuccess (Just [], _) -> flagFailure "Please check at least one Code of Conduct violation."
        FormSuccess (Just reasons, message) -> lookupPostMode >>= \case
            Just PostMode -> do
                let comment_handler_info = make_comment_handler_info def
                    permalink_route = comment_route_edit (commentHandlerRoutes comment_handler_info) comment_id
                permalink_route_text <- getUrlRender <*> pure permalink_route
                success <- runSYDB (flagCommentDB comment_id permalink_route_text user_id reasons message)
                if success
                    then alertSuccess "comment hidden and flagged for revision"
                    else alertDanger "error: another user flagged this just before you"
                return Nothing
            _ -> do
                now <- liftIO getCurrentTime
                let form = generateFlagCommentForm (Just (Just reasons)) (Just message)
                    flagging = CommentFlagging now user_id comment_id message
                (comment_widget, _) <-
                    makeCommentActionWidget
                      can_flag
                      mempty
                      comment
                      user
                      make_comment_handler_info
                      (def { mod_flag_map = M.insert comment_id (flagging, reasons) })
                      maxDepth
                      True
                return (Just (comment_widget, form))

        FormFailure errs -> flagFailure (T.intercalate ", " errs)
        _ -> flagFailure "Form missing."
  where
    flagFailure :: Text -> Handler a
    flagFailure msg = do
        alertDanger msg
        Just route <- getCurrentRoute
        redirect route

data CommentPostResult a b = ConfirmedPost a | Preview b

-- | Handle a POST to either a /reply or /d URL (reply, or new topic).
-- Checks the POST params for "mode" key - could either be a "post" (posts
-- the comment and returns its id) or a "preview" (returns the comment tree
-- and form, to wrap in a preview). Permission checking should occur *PRIOR
-- TO* this function.
postNewComment
    :: Maybe CommentId
    -> Entity User
    -> DiscussionId
    -> MakeActionPermissionsMap
    -> Handler
           (CommentPostResult (Either Text CommentId)
                              (Widget, Widget))
postNewComment
        mparent_id
        (Entity user_id user)
        discussion_id
        make_permissions_map = do
    -- commentReplyForm is OK here (the alternative is commentNewTopicForm)
    -- because they're actually the same form with different titles.
    ((result, _), _) <- runFormPost commentReplyForm
    case result of
        FormSuccess (NewComment contents visibility language) -> lookupPostMode >>= \case
            Just PostMode ->
                if userIsEstablished user
                    then do
                        ecomment_id <- runSDB $ postApprovedCommentDB
                            user_id mparent_id discussion_id contents
                            visibility language
                        case ecomment_id of
                            Left err -> return $ ConfirmedPost $ Left err
                            Right comment_id -> do
                                alertSuccess "comment posted"
                                return $ ConfirmedPost $ Right comment_id
                    else do
                        ecomment_id <- runSDB $ postUnapprovedCommentDB
                            user_id mparent_id discussion_id contents
                            visibility language
                        case ecomment_id of
                            Left err -> return $ ConfirmedPost $ Left err
                            Right comment_id -> do
                                alertSuccess "comment submitted for moderation"
                                return $ ConfirmedPost $ Right comment_id
            _ -> do
                let fetchCommentData c = do
                        closure <- fetchCommentAncestorClosuresDB' c
                        retract <- fetchCommentAncestorRetractsDB' c
                        depth <- fetchCommentDepthDB c
                        return (closure, retract, depth + 1)

                (earlier_closures, earlier_retracts, depth) <- runDB
                    (fmap (fromMaybe ([], [], 0))
                          (sequence (fmap fetchCommentData mparent_id)))

                (form, _) <-
                    generateFormPost
                        (commentForm
                            (maybe "New Topic"
                                   (const "Reply")
                                   mparent_id)
                            (Just contents))
                now <- liftIO getCurrentTime

                let (approved_ts, approved_by) =
                        if userIsEstablished user
                            then (Just now, Just user_id)
                            else (Nothing, Nothing)
                    comment =
                        Entity (key $ PersistInt64 0)
                               (Comment now
                                        approved_ts
                                        approved_by
                                        discussion_id
                                        mparent_id
                                        user_id
                                        contents
                                        depth
                                        visibility
                                        language)

                max_depth <- getMaxDepthDefault 0

                let comment_tree =
                        commentTreeWidget
                          (Tree.singleton comment)
                          (Just user_id)
                          dummyCommentRoutes -- 'True' below, so routes aren't used.
                          make_permissions_map
                          earlier_closures
                          earlier_retracts
                          (M.singleton user_id user)
                          mempty -- closure map
                          mempty -- retract map
                          mempty -- ticket map - TODO: this isn't right... if *this* comment is a ticket, we should display it as such.
                          mempty -- claim map
                          mempty -- flag map
                          True
                          max_depth
                          0
                          mempty

                return (Preview (comment_tree, form))
        FormMissing -> error "Form missing."
        FormFailure msgs -> error $ "Error submitting form: " ++ T.unpack (T.intercalate "\n" msgs)

postRethreadComment :: UserId -> CommentId -> Comment -> Handler Html
postRethreadComment user_id comment_id comment = do
    -- TODO: AVOID CYCLES

    ((result, _), _) <- runFormPost rethreadCommentForm
    case result of
        FormSuccess (new_parent_url, reason) -> do
            app <- getYesod
            let splitPath  = drop 1 . T.splitOn "/"
                stripQuery = fst . T.break (== '?')
                stripRoot  = fromMaybe new_parent_url . T.stripPrefix (appRoot $ appSettings app)
                url        = splitPath $ stripQuery (stripRoot new_parent_url)

            let notfound = error "could not find discussion for that URL"

            -- FIXME: We shouldn't have to enumerate the routes like this.
            -- Luckily robust rethreading is not priority.
            (new_route, mnew_parent_id, new_discussion_id) <- case parseRoute (url, []) of
                Just (new_route@(WikiCommentR new_project_handle new_language new_target new_parent_id)) -> do
                    new_discussion_id <-
                        maybe notfound (wikiPageDiscussion . entityVal) <$>
                          runDB (fetchProjectWikiPageByNameDB new_project_handle new_language new_target)
                    return (new_route, Just new_parent_id, new_discussion_id)
                Just (new_route@(WikiDiscussionR new_project_handle new_language new_target)) -> do
                    new_discussion_id <-
                        maybe notfound (wikiPageDiscussion . entityVal) <$>
                          runDB (fetchProjectWikiPageByNameDB new_project_handle new_language new_target)
                    return (new_route, Nothing, new_discussion_id)
                Just (new_route@(ProjectCommentR new_project_handle new_parent_id)) -> do
                    new_discussion_id <-
                        maybe notfound (projectDiscussion . entityVal) <$>
                          runDB (getBy (UniqueProjectHandle new_project_handle))
                    return (new_route, Just new_parent_id, new_discussion_id)
                Just (new_route@(ProjectDiscussionR new_project_handle)) -> do
                    new_discussion_id <-
                        maybe notfound (projectDiscussion . entityVal) <$>
                          runDB (getBy (UniqueProjectHandle new_project_handle))
                    return (new_route, Nothing, new_discussion_id)

                Nothing -> error "failed to parse URL"
                _       -> notfound

            let mold_parent_id = commentParent comment
            when (mnew_parent_id == mold_parent_id && new_discussion_id == commentDiscussion comment) $ do
                alertDanger "trying to move comment to its current location"
                getCommentDirectLinkR comment_id

            new_parent_depth <- maybe (return (-1)) fetchCommentDepth404DB mnew_parent_id
            old_parent_depth <- maybe (return (-1)) fetchCommentDepth404DB mold_parent_id

            lookupPostMode >>= \case
                Just PostMode -> do
                    runSDB $
                        rethreadCommentDB
                          mnew_parent_id
                          new_discussion_id
                          comment_id
                          user_id
                          reason
                          (old_parent_depth - new_parent_depth)

                    new_route_text <- getUrlRender <*> pure new_route
                    alertSuccess ("comment rethreaded to " <> new_route_text)
                    redirect new_parent_url

                _ -> error "no preview for rethreads yet" -- TODO
        _ -> error "Error when submitting form."

-- | Handle a POST to a /close URL.
-- Permission checking should occur *PRIOR TO* this function.
postRetractComment
        :: Entity User
        -> CommentId
        -> Comment
        -> (CommentMods -> CommentHandlerInfo)
        -> Handler (Maybe (Widget, Widget))
postRetractComment user comment_id comment make_comment_handler_info = do
    maxDepth <- getMaxDepthDefault 0
    ((result, _), _) <- runFormPost (retractCommentForm Nothing)
    case result of
        FormSuccess (NewClosure reason) -> do
            now <- liftIO getCurrentTime
            let retracting = CommentRetracting now reason comment_id
            lookupPostMode >>= \case
                Just PostMode -> do
                    runDB (insert_ retracting)
                    return Nothing
                _ -> do
                    (form, _) <- generateFormPost (retractCommentForm (Just reason))
                    (comment_widget, _) <-
                        makeCommentActionWidget
                          can_retract
                          mempty
                          (Entity comment_id comment)
                          user
                          make_comment_handler_info
                          (def { mod_retract_map = M.insert comment_id retracting })
                          maxDepth
                          True

                    return (Just (comment_widget, form))
        _ -> error "Error when submitting form."

postUnclaimComment
    :: Entity User
    -> CommentId
    -> Comment
    -> (CommentMods -> CommentHandlerInfo)
    -> Handler (Maybe (Widget, Widget))
postUnclaimComment user comment_id comment make_comment_handler_info = do
    maxDepth <- getMaxDepthDefault 0
    ((result, _), _) <- runFormPost (claimCommentForm Nothing)
    case result of
        FormSuccess mnote ->
            lookupPostMode >>= \case
                Just PostMode -> do
                    runSDB (userUnclaimCommentDB comment_id mnote)
                    return Nothing
                _ -> do
                    (form, _) <- generateFormPost (claimCommentForm (Just mnote))
                    (comment_widget, _) <-
                        makeCommentActionWidget
                        can_unclaim
                        mempty
                        (Entity comment_id comment)
                        user
                        make_comment_handler_info
                        (def { mod_claim_map = M.delete comment_id })
                        maxDepth
                        True
                    return (Just (comment_widget, form))
        _ -> error "Error when submitting form."


postWatchComment :: UserId -> CommentId -> Handler ()
postWatchComment viewer_id comment_id = do
    ((result, _), _) <- runFormPost watchCommentForm

    now <- liftIO getCurrentTime

    case result of
        FormSuccess () ->
            runYDB $ insert_ $ WatchedSubthread now viewer_id comment_id

        _ -> error "Error when submitting form."

postUnwatchComment :: UserId -> CommentId -> Handler ()
postUnwatchComment viewer_id comment_id = do
    ((result, _), _) <- runFormPost watchCommentForm

    case result of
        FormSuccess () ->
            runYDB $ delete $ from $ \ws ->
                where_ $ ws ^. WatchedSubthreadUser ==. val viewer_id
                    &&. ws ^. WatchedSubthreadRoot ==. val comment_id

        _ -> error "Error when submitting form."


getCommentTags :: CommentId -> Handler Html
getCommentTags comment_id = do
    muser_id <- maybeAuthId
    tags <- runDB $ M.findWithDefault [] comment_id <$> (fetchCommentCommentTagsDB comment_id >>= buildAnnotatedCommentTagsDB muser_id)
    defaultLayout $(widgetFile "tags")

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

    ((result_create, _), _) <- runFormPost createCommentTagForm
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

getCommentDirectLinkR :: CommentId -> Handler ()
getCommentDirectLinkR comment_id = do
    langs <- getLanguages
    route <- runDB $ makeCommentRouteDB langs comment_id

    maybe notFound redirect route

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
-- /c/#CommentId

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

postCommentTagR :: CommentId -> TagId -> Handler ()
postCommentTagR comment_id tag_id = do
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
                0 -> delete $ from $ \ct -> where_ $ ct ^. CommentTagId ==. val comment_tag_id
                x -> void $ update $ \ct -> do
                    set ct [ CommentTagCount =. val x ]
                    where_ $ ct ^. CommentTagId ==. val comment_tag_id

    getCommentDirectLinkR comment_id


