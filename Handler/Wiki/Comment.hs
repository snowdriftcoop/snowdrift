-- | Handler for comments on Wiki pages. Section comments are relative to /p/#handle/w/#target/c/#comment

module Handler.Wiki.Comment where

import Import

import qualified Data.Tree.Extra           as Tree
import           Handler.Comment
import           Model.Comment
import           Model.Project             (getProjectTagList)
import           Model.Tag
import           Model.User
import           Model.Wiki.Comment
import           Model.Wiki.Comment.Sql
import           Widgets.Preview
import           Widgets.Tag
import           View.Comment
import           View.Wiki.Comment

import           Data.Default              (def)
import qualified Data.Map                  as M
import qualified Data.Text                 as T
import qualified Data.Tree                 as Tree
import           Yesod.Default.Config
import           Yesod.Markdown


--------------------------------------------------------------------------------
-- Utility functions

-- | Convenience method for all pages that accept a project handle, target, and comment id
-- as URL parameters. Makes sure that the comment is indeed on the page. Redirects if the
-- comment was rethreaded. 404's if the comment doesn't exist. 403 if permission denied.
checkCommentPage :: Text -> Text -> CommentId -> Handler (Maybe (Entity User), Entity Project, Entity WikiPage, Comment)
checkCommentPage project_handle target comment_id = do
    muser <- maybeAuth
    (project, page, comment) <- checkCommentPage' (entityKey <$> muser) project_handle target comment_id
    return (muser, project, page, comment)

-- | Like checkCommentPage, but authentication is required.
checkCommentPageRequireAuth :: Text -> Text -> CommentId -> Handler (Entity User, Entity Project, Entity WikiPage, Comment)
checkCommentPageRequireAuth project_handle target comment_id = do
    user@(Entity user_id _) <- requireAuth
    (project, page, comment) <- checkCommentPage' (Just user_id) project_handle target comment_id
    return (user, project, page, comment)

-- | Abstract checkCommentPage and checkCommentPageRequireAuth. You shouldn't
-- use this function directly.
checkCommentPage' :: Maybe UserId -> Text -> Text -> CommentId -> Handler (Entity Project, Entity WikiPage, Comment)
checkCommentPage' muser_id project_handle target comment_id = do
    redirectIfRethreaded comment_id

    (project, page, ecomment) <- runYDB $ do
        project@(Entity project_id _) <- getBy404 (UniqueProjectHandle project_handle)
        page <- getBy404 (UniqueWikiTarget (entityKey project) target)
        let has_permission = exprCommentWikiPagePermissionFilter muser_id (val project_id)
        ecomment <- fetchCommentDB comment_id has_permission
        return (project, page, ecomment)

    case ecomment of
        Left CommentNotFound         -> notFound
        Left CommentPermissionDenied -> permissionDenied "You don't have permission to view this comment."
        Right comment                -> return (project, page, comment)

requireModerator :: Text -> Text -> UserId -> Handler ()
requireModerator message project_handle user_id = do
    ok <- runYDB $ do
        Entity project_id _ <- getBy404 (UniqueProjectHandle project_handle)
        userIsProjectModeratorDB user_id project_id
    unless ok $
        permissionDenied message

-- | Either or preview or post a new Comment, which Maybe has a parent, depending on if it's
-- a reply or a new topic.
processWikiComment :: Maybe Text -> Maybe CommentId -> Markdown -> Project -> WikiPage -> Handler Html
processWikiComment mode =
    case mode of
        Just "preview" -> processWikiCommentPreview
        Just "post"    -> processWikiCommentPost
        _              -> error $ "Error: unrecognized mode (" ++ show mode ++ ")"

processWikiCommentPreview :: Maybe CommentId -> Markdown -> Project -> WikiPage -> Handler Html
processWikiCommentPreview maybe_parent_id text project page = do
    Entity user_id user <- requireAuth

    earlier_closures <- runDB (maybe (return []) fetchCommentAncestorClosuresDB' maybe_parent_id)

    (form, _) <- generateFormPost $ commentForm (maybe "New Topic" (const "Reply") maybe_parent_id) (Just text)

    depth <- depthFromMaybeParentId maybe_parent_id
    now <- liftIO getCurrentTime
    let (moderated_ts, moderated_by) = if userIsEstablished user then (Just now, Just user_id) else (Nothing, Nothing)
        comment = Entity
                    (Key $ PersistInt64 0)
                    (Comment now moderated_ts moderated_by (wikiPageDiscussion page) maybe_parent_id user_id text depth)

    max_depth <- getMaxDepthDefault 0
    let project_handle = projectHandle project
        target         = wikiPageTarget page
        comment_widget = previewWidget form "post" $
            commentTreeWidget
              mempty
              (Tree.singleton comment)
              (Just user_id)
              (wikiPageCommentRoutes project_handle target)
              (makeWikiPageCommentActionPermissions project_handle target)
              earlier_closures
              (M.singleton user_id user)
              mempty -- closure map
              mempty -- ticket map - TODO(mitchell): this isn't right... if *this* comment is a ticket, we must display it as such.
              mempty -- flag map
              True
              max_depth
              0

    defaultLayout $(widgetFile "wiki_comment_wrapper")

processWikiCommentPost :: Maybe CommentId -> Markdown -> Project -> WikiPage -> Handler Html
processWikiCommentPost maybe_parent_id text project page = do
    Entity user_id user <- requireAuth
    now <- liftIO getCurrentTime
    depth <- depthFromMaybeParentId maybe_parent_id

    let is_established = userIsEstablished user
    maybe_parent_id' <- runSYDB $ do
        maybe_parent_id' <- lift $ maybe (return Nothing) (fmap Just . fetchCommentDestinationDB) maybe_parent_id

        comment_id <-
            if is_established
                then insertApprovedCommentDB   now now user_id (wikiPageDiscussion page) maybe_parent_id' user_id text depth
                else insertUnapprovedCommentDB now             (wikiPageDiscussion page) maybe_parent_id' user_id text depth

        let content = T.lines $ (\ (Markdown str) -> str) text
            tickets = map T.strip $ mapMaybe (T.stripPrefix "ticket:") content
            tags    = map T.strip $ mconcat $ map (T.splitOn ",") $ mapMaybe (T.stripPrefix "tags:") content

        lift $ forM_ tickets $ \ticket -> insert_ $ Ticket now now ticket comment_id
        lift $ forM_ tags $ \tag -> do
            tag_id <- fmap (either entityKey id) $ insertBy $ Tag tag
            insert_ $ CommentTag comment_id tag_id user_id 1

        ancestor_ids <- lift $ maybe (return [])
                                      (\parent_id -> (parent_id :) <$> fetchCommentAncestorsDB parent_id)
                                      maybe_parent_id

        lift $ forM_ ancestor_ids (insert_ . CommentAncestor comment_id)

        lift $ update $ \ticket -> do
                  set ticket [ TicketUpdatedTs =. val now ]
                  where_ $ ticket ^. TicketComment `in_` subFetchCommentAncestorsDB comment_id

        return maybe_parent_id'

    alertSuccess $ if is_established then "comment posted" else "comment submitted for moderation"
    redirect $ maybe (DiscussWikiR (projectHandle project) (wikiPageTarget page)) (WikiCommentR (projectHandle project) (wikiPageTarget page)) maybe_parent_id'

-- Get the depth of a comment, given (maybe) its parent's CommentId.
depthFromMaybeParentId :: Maybe CommentId -> Handler Int
depthFromMaybeParentId = maybe (return 0) (\c ->  fmap (+1) $ runDB (fetchCommentDepthDB c))

--------------------------------------------------------------------------------
-- / and /reply

getWikiCommentR :: Text -> Text -> CommentId -> Handler Html
getWikiCommentR project_handle target comment_id = do
    (muser, Entity project_id _, _, comment) <- checkCommentPage project_handle target comment_id
    (comment_widget, comment_tree) <-
        makeWikiPageCommentTreeWidget
          muser
          project_id
          project_handle
          target
          (Entity comment_id comment)
          def
          getMaxDepth
          False
          mempty

    case muser of
        Nothing -> return ()
        Just (Entity user_id _) -> runYDB $ do
            ok <- userIsWatchingProjectDB user_id project_id
            when ok $
                userViewCommentsDB user_id (map entityKey (Tree.flatten comment_tree))

    defaultLayout $(widgetFile "wiki_comment_wrapper")

getReplyWikiCommentR :: Text -> Text -> CommentId -> Handler Html
getReplyWikiCommentR project_handle target comment_id = do
    (user, Entity project_id _, _, comment) <- checkCommentPageRequireAuth project_handle target comment_id
    comment_widget <-
        makeWikiPageCommentTreeWidget'
          (Just user)
          project_id
          project_handle
          target
          (Entity comment_id comment)
          def
          getMaxDepth
          False
          commentReplyFormWidget
    defaultLayout $(widgetFile "wiki_comment_wrapper")

postReplyWikiCommentR :: Text -> Text -> CommentId -> Handler Html
postReplyWikiCommentR project_handle target comment_id = do
    (_, Entity _ project, Entity _ page, _) <- checkCommentPage project_handle target comment_id

    ((result, _), _) <- runFormPost commentReplyForm

    case result of
        FormSuccess text -> do
            mode <- lookupPostParam "mode"
            processWikiComment mode (Just comment_id) text project page
        FormMissing      -> error "Form missing."
        FormFailure msgs -> error $ "Error submitting form: " ++ T.unpack (T.intercalate "\n" msgs)

--------------------------------------------------------------------------------
-- /delete

getDeleteWikiCommentR :: Text -> Text -> CommentId -> Handler Html
getDeleteWikiCommentR project_handle target comment_id = do
    (user, Entity project_id _, _, comment) <- checkCommentPageRequireAuth project_handle target comment_id
    comment_widget <-
        makeWikiPageCommentTreeWidget'
          (Just user)
          project_id
          project_handle
          target
          (Entity comment_id comment)
          def
          (getMaxDepthDefault 0)
          False
          widget
    defaultLayout $(widgetFile "wiki_comment_wrapper")
  where
    widget = [whamlet|
        <div>
            <form method=POST>
                <input type=submit name=mode value=Delete>
                <input type=submit name=mode value=Cancel>
    |]

postDeleteWikiCommentR :: Text -> Text -> CommentId -> Handler Html
postDeleteWikiCommentR project_handle target comment_id =
    lookupPostParam "mode" >>= \case
        Just "Delete" -> deleteDeleteWikiCommentR project_handle target comment_id
        _             -> redirect (WikiCommentR project_handle target comment_id)

deleteDeleteWikiCommentR :: Text -> Text -> CommentId -> Handler Html
deleteDeleteWikiCommentR project_handle target comment_id = do
    user_id <- requireAuthId
    comment <- runYDB $ get404 comment_id

    ok <- runDB $ do
        can_delete <- userCanDeleteCommentDB user_id (Entity comment_id comment)
        if can_delete
            then deleteCommentDB comment_id >> return True
            else return False

    if ok
        then alertSuccess "comment deleted" >> redirect (DiscussWikiR project_handle target)
        else permissionDenied "You can't delete that comment."

--------------------------------------------------------------------------------
-- /edit

getEditWikiCommentR :: Text -> Text -> CommentId -> Handler Html
getEditWikiCommentR project_handle target comment_id = do
    (user, Entity project_id _, _, comment) <- checkCommentPageRequireAuth project_handle target comment_id
    comment_widget <-
        makeWikiPageCommentTreeWidget'
          (Just user)
          project_id
          project_handle
          target
          (Entity comment_id comment)
          def
          (getMaxDepthDefault 0)
          False
          (commentEditFormWidget (commentText comment))
    defaultLayout $(widgetFile "wiki_comment_wrapper")

postEditWikiCommentR :: Text -> Text -> CommentId -> Handler Html
postEditWikiCommentR project_handle target comment_id = do
    (user@(Entity user_id _), Entity project_id _, _, comment) <- checkCommentPageRequireAuth project_handle target comment_id
    ((result, _), _) <- runFormPost $ commentEditForm ""
    case result of
        FormSuccess new_text -> lookupPostParam "mode" >>= \case
            Just "preview" -> previewEdit user project_id comment new_text
            Just "post"    -> postEdit user_id comment new_text
            m              -> error $ "Error: unrecognized mode (" ++ show m ++ ")"
        FormMissing -> error "Form missing."
        FormFailure msgs -> error $ "Error submitting form: " ++ T.unpack (T.intercalate "\n" msgs)
  where
    previewEdit :: Entity User -> ProjectId -> Comment -> Markdown -> Handler Html
    previewEdit user project_id comment new_text = do
        (form, _) <- generateFormPost $ commentEditForm new_text
        comment_widget <- previewWidget form "post" <$>
            makeWikiPageCommentTreeWidget'
              (Just user)
              project_id
              project_handle
              target
              (Entity comment_id comment)
              mods
              (getMaxDepthDefault 0)
              True
              mempty
        defaultLayout $(widgetFile "wiki_comment_wrapper")
      where
        mods :: CommentMods
        mods = def
            { mod_comment = \c -> c { commentText = new_text }
            -- Since an edit removes a flagging, don't show the flagged markup in preview.
            , mod_flag_map = M.delete comment_id
            }

    postEdit :: UserId -> Comment -> Markdown -> Handler Html
    postEdit user_id comment new_text = do
        unless (userCanEditComment user_id comment) $
            permissionDenied "You can't edit that comment."

        runSYDB (editCommentDB comment_id new_text)

        alertSuccess "posted new edit"
        redirect (WikiCommentR project_handle target comment_id)

--------------------------------------------------------------------------------
-- /flag

getFlagWikiCommentR :: Text -> Text -> CommentId -> Handler Html
getFlagWikiCommentR project_handle target comment_id = do
    (user, Entity project_id _, _, comment) <- checkCommentPageRequireAuth project_handle target comment_id
    comment_widget <-
        makeWikiPageCommentTreeWidget'
          (Just user)
          project_id
          project_handle
          target
          (Entity comment_id comment)
          def
          (getMaxDepthDefault 0)
          False
          widget
    defaultLayout $(widgetFile "wiki_comment_wrapper")
  where
    widget = do
        (form, enctype) <- handlerToWidget $ generateFormPost (flagCommentForm Nothing Nothing)
        [whamlet|
            <form method="POST" enctype=#{enctype}>
                <h4>Code of Conduct Violation(s):
                ^{form}
                <input type="submit" value="preview flag message">
                <input type="hidden" name="mode" value="preview">
        |]

postFlagWikiCommentR :: Text -> Text -> CommentId -> Handler Html
postFlagWikiCommentR project_handle target comment_id = do
    (user@(Entity user_id _), Entity project_id _, _, comment) <- checkCommentPageRequireAuth project_handle target comment_id
    ((result, _), _) <- runFormPost (flagCommentForm Nothing Nothing)
    case result of
        -- TODO(mitchell): Change the form to just return [FlagReason], not Maybe [FlagReason]
        FormSuccess (Nothing, _) -> flagFailure "Please check at least one Code of Conduct violation."
        FormSuccess (Just [], _) -> flagFailure "Please check at least one Code of Conduct violation."
        FormSuccess (Just reasons, message) -> lookupPostParam "mode" >>= \case
            Just "preview"      -> previewFlag user project_id comment reasons message
            Just "flag comment" -> postFlag user_id reasons message
            m                   -> error $ "Error: unrecognized mode (" ++ show m ++ ")"
        FormFailure errs -> flagFailure (T.intercalate ", " errs)
        _ -> flagFailure "Form missing."
  where
    previewFlag :: Entity User -> ProjectId -> Comment -> [FlagReason] -> Maybe Markdown -> Handler Html
    previewFlag user project_id comment reasons message = do
        (form, _) <- generateFormPost $ flagCommentForm (Just (Just reasons)) (Just message)

        let mods = def { mod_flag_map = M.insert comment_id (message, reasons) }
            style_widget =
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

        comment_widget <- (style_widget <>) . previewWidget form_with_header "flag comment" <$>
            makeWikiPageCommentTreeWidget'
              (Just user)
              project_id
              project_handle
              target
              (Entity comment_id comment)
              mods
              (getMaxDepthDefault 0)
              False
              mempty

        defaultLayout $(widgetFile "wiki_comment_wrapper")

    postFlag :: UserId -> [FlagReason] -> Maybe Markdown -> Handler Html
    postFlag user_id reasons message = do
            let permalink_route = comment_route_edit (wikiPageCommentRoutes project_handle target) comment_id
            permalink_route_text <- getUrlRender <*> pure permalink_route
            success <- runSYDB (flagCommentDB comment_id permalink_route_text user_id reasons message)
            if success
                then alertSuccess "comment hidden and flagged for revision"
                else alertDanger "error: another user flagged this just before you"
            redirect (DiscussWikiR project_handle target)

flagFailure :: Text -> Handler a
flagFailure msg = do
    alertDanger msg
    Just route <- getCurrentRoute
    redirect route

--------------------------------------------------------------------------------
-- /moderate

getApproveWikiCommentR :: Text -> Text -> CommentId -> Handler Html
getApproveWikiCommentR project_handle target comment_id = do
    (user@(Entity user_id _), Entity project_id _, _, comment) <- checkCommentPageRequireAuth project_handle target comment_id
    requireModerator "You must be a moderator to approve posts." project_handle user_id

    comment_widget <-
        makeWikiPageCommentTreeWidget'
          (Just user)
          project_id
          project_handle
          target
          (Entity comment_id comment)
          def
          getMaxDepth
          False
          widget

    defaultLayout $(widgetFile "wiki_comment_wrapper")
  where
    widget = [whamlet|
        <form method="POST">
            <input type=submit value="approve post">
    |]

postApproveWikiCommentR :: Text -> Text -> CommentId -> Handler Html
postApproveWikiCommentR project_handle target comment_id = do
    (Entity user_id _, _, _, comment) <- checkCommentPageRequireAuth project_handle target comment_id
    requireModerator "You must be a moderator to approve posts." project_handle user_id

    runSDB (approveCommentDB user_id comment_id comment)
    alertSuccess "comment approved"
    redirect (WikiCommentR project_handle target comment_id)

--------------------------------------------------------------------------------
-- /close and /retract

getRetractWikiCommentR :: Text -> Text -> CommentId -> Handler Html
getRetractWikiCommentR project_handle target comment_id = do
    (user, project_id, comment) <- retractSanityCheck project_handle target comment_id

    comment_widget <-
        makeWikiPageCommentTreeWidget'
          (Just user)
          project_id
          project_handle
          target
          (Entity comment_id comment)
          def
          getMaxDepth
          False
          (commentRetractFormWidget Nothing)

    defaultLayout $(widgetFile "wiki_comment_wrapper")

retractSanityCheck :: Text -> Text -> CommentId -> Handler (Entity User, ProjectId, Comment)
retractSanityCheck project_handle target comment_id = do
    (user@(Entity user_id _), Entity project_id _, _, comment) <- checkCommentPageRequireAuth project_handle target comment_id
    unless (commentUser comment == user_id) $
        permissionDenied "You can only retract your own comments."
    return (user, project_id, comment)

getCloseWikiCommentR :: Text -> Text -> CommentId -> Handler Html
getCloseWikiCommentR project_handle target comment_id = do
    (user, project_id, comment) <- closeSanityCheck project_handle target comment_id

    comment_widget <-
        makeWikiPageCommentTreeWidget'
          (Just user)
          project_id
          project_handle
          target
          (Entity comment_id comment)
          def
          getMaxDepth
          False
          (commentCloseFormWidget Nothing)

    defaultLayout $(widgetFile "wiki_comment_wrapper")

closeSanityCheck :: Text -> Text -> CommentId -> Handler (Entity User, ProjectId, Comment)
closeSanityCheck project_handle target comment_id = do
    (u@(Entity _ user), Entity project_id _, _, comment) <- checkCommentPageRequireAuth project_handle target comment_id
    unless (userCanCloseComment user) $
        permissionDenied "You must be an established user to close a conversation."
    return (u, project_id, comment)

postRetractWikiCommentR, postCloseWikiCommentR :: Text -> Text -> CommentId -> Handler Html
postRetractWikiCommentR = postClosureWikiComment retractSanityCheck commentRetractForm newRetractedCommentClosure "retract"
postCloseWikiCommentR   = postClosureWikiComment closeSanityCheck   commentCloseForm   newClosedCommentClosure    "close"

-- | POST handler for either closing or retracting a comment.
postClosureWikiComment :: (Text -> Text -> CommentId -> Handler (Entity User, ProjectId, Comment))
                       -> (Maybe Markdown -> Form Markdown)
                       -> (UserId -> Markdown -> CommentId -> Handler CommentClosure)
                       -> Text
                       -> Text
                       -> Text
                       -> CommentId
                       -> Handler Html
postClosureWikiComment sanity_check make_closure_form make_new_comment_closure action project_handle target comment_id = do
    (user@(Entity user_id _), project_id, comment) <- sanity_check project_handle target comment_id
    ((result, _), _) <- runFormPost $ make_closure_form Nothing
    case result of
        FormSuccess reason -> do
            new_comment_closure <- make_new_comment_closure user_id reason comment_id
            lookupPostParam "mode" >>= \case
                Just "preview" -> do
                    (form, _) <- generateFormPost $ make_closure_form (Just reason)

                    let mods = def { mod_closure_map = M.insert comment_id new_comment_closure }
                    comment_widget <- previewWidget form action <$>
                        makeWikiPageCommentTreeWidget'
                          (Just user)
                          project_id
                          project_handle
                          target
                          (Entity comment_id comment)
                          mods
                          (getMaxDepthDefault 0)
                          True
                          mempty

                    defaultLayout $(widgetFile "wiki_comment_wrapper")
                Just mode | mode == action -> do
                    runDB $ insert_ new_comment_closure
                    redirect $ WikiCommentR project_handle target comment_id
                mode -> error $ "Error: unrecognized mode (" ++ show mode ++ ")"
        _ -> error "Error when submitting form."

--------------------------------------------------------------------------------
-- /rethread

getRethreadWikiCommentR :: Text -> Text -> CommentId -> Handler Html
getRethreadWikiCommentR _ _ _ = do
    (form, _) <- generateFormPost rethreadForm
    defaultLayout $(widgetFile "rethread")

postRethreadWikiCommentR :: Text -> Text -> CommentId -> Handler Html
postRethreadWikiCommentR project_handle target comment_id = do
    -- TODO(david): AVOID CYCLES

    (Entity user_id _, Entity project_id _, _, comment) <- checkCommentPageRequireAuth project_handle target comment_id

    ok <- runDB (userIsProjectModeratorDB user_id project_id)
    unless ok $
        permissionDenied "You must be a moderator to rethread."

    ((result, _), _) <- runFormPost rethreadForm
    case result of
        FormSuccess (new_parent_url, reason) -> do
            app <- getYesod
            let splitPath  = drop 1 . T.splitOn "/"
                stripQuery = fst . T.break (== '?')
                stripRoot  = fromMaybe new_parent_url . T.stripPrefix (appRoot $ settings app)
                url        = splitPath $ stripQuery $ stripRoot new_parent_url

            (new_parent_id, new_discussion_id) <- case parseRoute (url, []) of
                Just (WikiCommentR new_project_handle new_target new_parent_id) -> do
                    new_discussion_id <- getNewDiscussionId user_id project_id new_project_handle new_target
                    return (Just new_parent_id, new_discussion_id)

                Just (DiscussWikiR new_project_handle new_target) -> do
                    new_discussion_id <- getNewDiscussionId user_id project_id new_project_handle new_target
                    return (Nothing, new_discussion_id)

                Nothing -> error "failed to parse URL"

                _ -> error "could not find discussion for that URL"

            let old_parent_id = commentParent comment
            when (new_parent_id == old_parent_id && new_discussion_id == commentDiscussion comment) $
                error "trying to move comment to its current location"

            new_parent_depth <- maybe (return $ -1) fetchCommentDepth404DB new_parent_id
            old_parent_depth <- maybe (return $ -1) fetchCommentDepth404DB old_parent_id

            let depth_offset = old_parent_depth - new_parent_depth

            mode <- lookupPostParam "mode"
            let action :: Text = "rethread"
            case mode of
                Just "preview" -> error "no preview for rethreads yet" -- TODO

                Just action' | action' == action -> do
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

                m -> error $ "Error: unrecognized mode (" ++ show m ++ ")"
        _ -> error "Error when submitting form."
  where
    getNewDiscussionId :: UserId -> ProjectId -> Text -> Text -> Handler DiscussionId
    getNewDiscussionId user_id project_id new_project_handle new_target = do
        Entity new_project_id _ <- getByErr "could not find project" $ UniqueProjectHandle new_project_handle
        when (new_project_id /= project_id) $
            requireModerator "You must be a moderator to rethread." new_project_handle user_id
        maybe (error "could not find new page") (wikiPageDiscussion . entityVal) <$>
            runDB (getBy $ UniqueWikiTarget new_project_id new_target)

--------------------------------------------------------------------------------
-- tag passthroughs, for better looking URLs

getWikiCommentTagsR       _ _ = getCommentTagsR
getWikiCommentTagR        _ _ = getCommentTagR
postWikiCommentTagR       _ _ = postCommentTagR
postWikiCommentApplyTagR  _ _ = postCommentApplyTagR
postWikiCommentCreateTagR _ _ = postCommentCreateTagR

--------------------------------------------------------------------------------
-- /tag/new

-- TODO(mitchell): This doesn't really belong here, but we need the project id
-- to call getProjectTagList. However, this assumes a Comment is on a Project,
-- which is not true in general. Need to build helper functions to handle the
-- case that a comment is/is not on a project, and generate 'add tag' views
-- accordingly.

getWikiCommentAddTagR :: Text -> Text -> CommentId -> Handler Html
getWikiCommentAddTagR project_handle target comment_id = do
    (Entity user_id user, Entity project_id _, _, _) <- checkCommentPageRequireAuth project_handle target comment_id
    unless (userIsEstablished user) $
        permissionDenied "You must be an established user to add tags"

    (tag_map, tags, project_tags, other_tags) <- runDB $ do
        comment_tags <- fetchCommentCommentTagsDB comment_id
        tag_map <- entitiesMap <$> fetchTagsInDB (map commentTagTag comment_tags)
        tags <- (M.! comment_id) <$> buildAnnotatedCommentTagsDB (Just user_id) comment_tags
        (project_tags, other_tags) <- getProjectTagList project_id
        return (tag_map, tags, project_tags, other_tags)

    let filter_tags = filter (\(Entity t _) -> not $ M.member t tag_map)
    (apply_form, _) <- generateFormPost $ newCommentTagForm (filter_tags project_tags) (filter_tags other_tags)
    (create_form, _) <- generateFormPost $ createCommentTagForm

    defaultLayout $(widgetFile "new_comment_tag")

--------------------------------------------------------------------------------
-- DEPRECATED

-- This is just because we used to have "/comment/#" with that longer URL,
-- and this keeps any permalinks from breaking
getOldDiscussCommentR :: Text -> Text -> CommentId -> Handler Html
getOldDiscussCommentR project_handle target comment_id = redirect $ WikiCommentR project_handle target comment_id
