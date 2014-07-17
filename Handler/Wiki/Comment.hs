-- | Handler for comments on Wiki pages. Section comments are relative to /p/#handle/w/#target/c/#comment

module Handler.Wiki.Comment where

import Import

import qualified Data.Tree.Extra           as Tree
import           Data.Tree.Extra           (sortTreeBy)
import           Model.AnnotatedTag
import           Model.Comment
import           Model.Project             (getProjectTagList)
import           Model.Tag                 (TagMap, getAllTags)
import           Model.User
import           Widgets.Preview
import           Widgets.Tag
import           View.Comment

import           Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import           Data.Default              (Default, def)
import qualified Data.Map                  as M
import qualified Data.Set                  as S
import qualified Data.Text                 as T
import           Network.HTTP.Types.Status (movedPermanently301)
import           Yesod.Default.Config
import           Yesod.Markdown


--------------------------------------------------------------------------------
-- Utility functions

redirectIfRethreaded :: Text -> CommentId -> Handler ()
redirectIfRethreaded project_handle comment_id = runDB go >>= \case
    Nothing -> return ()
    Just (destination_comment_id, target) ->
        redirectWith movedPermanently301 (DiscussCommentR project_handle target destination_comment_id)
  where
    go :: YesodDB App (Maybe (CommentId, Text))
    go = runMaybeT $ do
        destination_comment_id <- MaybeT (getCommentRethread comment_id)
        target                 <- lift (wikiPageTarget <$> getCommentPage destination_comment_id)
        return (destination_comment_id, target)

-- | Convenience method for all pages that accept a project handle, target, and comment id
-- as URL parameters. Makes sure that the comment is indeed on the page. Redirects if the
-- comment was rethreaded.
checkCommentPage :: Text -> Text -> CommentId -> Handler (Entity Project, Entity WikiPage, Comment)
checkCommentPage project_handle target comment_id = do
    redirectIfRethreaded project_handle comment_id

    (project@(Entity project_id _), page, comment) <- runDB $ do
        project <- getBy404 $ UniqueProjectHandle project_handle
        page    <- getBy404 $ UniqueWikiTarget (entityKey project) target
        comment <- get404 comment_id
        return (project, page, comment)

    when (commentDiscussion comment /= wikiPageDiscussion (entityVal page)) $
        error "comment does not match page"

    -- The logic here is DUPLICATED (in SQL land) in Model.Comment.exprPermissionFilter!
    -- (because this function only fetches the root comment via Database.Persist.get) - all
    -- changes here must be reflected there, too!

    maybeAuthId >>= \case
        -- Not logged in: only show approved-and-not-flagged comments
        Nothing -> do
            ok <- isApprovedAndNotFlagged comment
            unless ok deny
        -- Logged in:
        Just viewer_id -> do
            -- If mod: no restrictions (show unapproved and flagged)
            is_mod <- runDB $ isProjectModerator' viewer_id project_id
            unless is_mod $
                -- Otherwise, if ordinary user, if viewing own comment, no restrictions.
                unless (commentUser comment == viewer_id) $ do
                    -- Otherwise, same restriction as not logged in applies:
                    -- only show approved-and-not-flagged comments.
                    ok <- isApprovedAndNotFlagged comment
                    unless ok deny

    return (project, page, comment)
  where
    isApprovedAndNotFlagged :: Comment -> Handler Bool
    isApprovedAndNotFlagged comment =
        if isApproved comment
            then not <$> runDB (isFlagged comment_id)
            else return False

    deny :: Handler a
    deny = permissionDenied "You don't have permission to view this comment."

requireModerator :: Text -> Text -> UserId -> Handler ()
requireModerator message project_handle user_id = do
    ok <- runDB $ isProjectModerator project_handle user_id
    unless ok $
        permissionDenied message

processWikiComment :: Maybe Text -> Maybe CommentId -> Markdown -> Entity Project -> WikiPage -> Handler Html
processWikiComment mode =
    case mode of
        Just "preview" -> processWikiCommentPreview
        Just "post"    -> processWikiCommentPost
        _              -> error $ "Error: unrecognized mode (" ++ show mode ++ ")"

-- TODO(mitchell): We should reuse makeCommentWidget here, somehow.
processWikiCommentPreview :: Maybe CommentId -> Markdown -> Entity Project -> WikiPage -> Handler Html
processWikiCommentPreview maybe_parent_id text (Entity _ project) page = do
    Entity user_id user <- requireAuth

    (earlier_closures, tag_map) <- runDB $ (,)
        <$> maybe (return []) getAncestorClosures' maybe_parent_id
        <*> (entitiesMap <$> getAllTags)

    depth <- depthFromMaybeParentId maybe_parent_id
    now <- liftIO getCurrentTime
    let comment =
          Entity (Key $ PersistInt64 0) $
            Comment now Nothing Nothing (wikiPageDiscussion page) maybe_parent_id user_id text depth

        rendered_comment = let project_handle = projectHandle project
                               target         = wikiPageTarget page
                               comment_widget = commentTreeWidget
                                                  mempty
                                                  (Tree.singleton comment)
                                                  earlier_closures
                                                  (M.singleton user_id user)
                                                  mempty -- closure map - TODO(mitchell): this isn't right...?
                                                  mempty -- ticket map - TODO(mitchell): this isn't right either
                                                  mempty -- flag map
                                                  tag_map
                                                  (projectHandle project)
                                                  (wikiPageTarget page)
                                                  False   -- show actions?
                                                  0
                                                  0
                           in $(widgetFile "comment_wrapper")

    (form, _) <- generateFormPost $
        commentForm
          (maybe "New Topic" (const "Reply") maybe_parent_id)
          (Just text)
    defaultLayout $ previewWidget form "post" rendered_comment

processWikiCommentPost :: Maybe CommentId -> Markdown -> Entity Project -> WikiPage -> Handler Html
processWikiCommentPost maybe_parent_id text (Entity _ project) page = do
    Entity user_id user <- requireAuth
    now <- liftIO getCurrentTime
    depth <- depthFromMaybeParentId maybe_parent_id

    let is_established = isEstablished user
    maybe_parent_id' <- runDB $ do
        maybe_parent_id' <- maybe (return Nothing) (fmap Just . getCommentDestination) maybe_parent_id

        comment_id <- insert $ Comment now
                                       (if is_established then Just now else Nothing)
                                       (if is_established then Just user_id else Nothing)
                                       (wikiPageDiscussion page)
                                       maybe_parent_id'
                                       user_id
                                       text
                                       depth

        let content = T.lines $ (\ (Markdown str) -> str) text
            tickets = map T.strip $ mapMaybe (T.stripPrefix "ticket:") content
            tags    = map T.strip $ mconcat $ map (T.splitOn ",") $ mapMaybe (T.stripPrefix "tags:") content

        forM_ tickets $ \ ticket -> insert_ $ Ticket now now ticket comment_id
        forM_ tags $ \ tag -> do
            tag_id <- fmap (either entityKey id) $ insertBy $ Tag tag
            insert_ $ CommentTag comment_id tag_id user_id 1

        ancestor_ids <- maybe (return [])
                              (\parent_id -> (parent_id :) <$> getCommentAncestors parent_id)
                              maybe_parent_id

        forM_ ancestor_ids (insert_ . CommentAncestor comment_id)

        update $ \ticket -> do
            set ticket [ TicketUpdatedTs =. val now ]
            where_ $ ticket ^. TicketComment `in_` subGetCommentAncestors comment_id

        return maybe_parent_id'

    addAlert "success" $ if is_established then "comment posted" else "comment submitted for moderation"
    redirect $ maybe (DiscussWikiR (projectHandle project) (wikiPageTarget page)) (DiscussCommentR (projectHandle project) (wikiPageTarget page)) maybe_parent_id'

-- Get the depth of a comment, given (maybe) its parent's CommentId.
depthFromMaybeParentId :: Maybe CommentId -> Handler Int
depthFromMaybeParentId = maybe (return 0) (fmap (+1) . runDB . getCommentDepth)

getMaxDepth :: Handler Int
getMaxDepth = getMaxDepthDefault 11 -- hard-coded default max depth

getMaxDepthZero :: Handler Int
getMaxDepthZero = getMaxDepthDefault 0

getMaxDepthDefault :: Int -> Handler Int
getMaxDepthDefault n = fromMaybe n <$> runInputGet (iopt intField "maxdepth")

--------------------------------------------------------------------------------
-- / and /reply

getDiscussCommentR :: Text -> Text -> CommentId -> Handler Html
getDiscussCommentR project_handle target comment_id = do
    comment_widget <- makeCommentWidget
                        getMaxDepth
                        True
                        mempty
                        project_handle
                        target
                        comment_id
    defaultLayout $(widgetFile "comment_wrapper")

getReplyCommentR :: Text -> Text -> CommentId -> Handler Html
getReplyCommentR project_handle target comment_id = do
    void requireAuth
    comment_widget <- makeCommentWidget
                        getMaxDepth
                        True
                        widget
                        project_handle
                        target
                        comment_id
    defaultLayout $(widgetFile "comment_wrapper")
  where
    widget = commentFormWidget "Reply" Nothing

postReplyCommentR :: Text -> Text -> CommentId -> Handler Html
postReplyCommentR project_handle target comment_id = do
    (project, Entity _ page, _) <- checkCommentPage project_handle target comment_id

    ((result, _), _) <- runFormPost commentReplyForm

    case result of
        FormSuccess text -> do
            mode <- lookupPostParam "mode"
            processWikiComment mode (Just comment_id) text project page
        FormMissing      -> error "Form missing."
        FormFailure msgs -> error $ "Error submitting form: " ++ T.unpack (T.intercalate "\n" msgs)

--------------------------------------------------------------------------------
-- /delete

getDeleteCommentR :: Text -> Text -> CommentId -> Handler Html
getDeleteCommentR project_handle target comment_id = do
    void requireAuth
    comment_widget <- makeCommentWidget
                        getMaxDepthZero
                        True
                        widget
                        project_handle
                        target
                        comment_id
    defaultLayout $(widgetFile "comment_wrapper")
  where
    widget = [whamlet|
        <div>
            <form method=POST>
                <input type=submit name=mode value=Delete>
                <input type=submit name=mode value=Cancel>
    |]

postDeleteCommentR :: Text -> Text -> CommentId -> Handler Html
postDeleteCommentR project_handle target comment_id =
    lookupPostParam "mode" >>= \case
        Just "Delete" -> deleteDeleteCommentR project_handle target comment_id
        _             -> redirect $ DiscussCommentR project_handle target comment_id

deleteDeleteCommentR :: Text -> Text -> CommentId -> Handler Html
deleteDeleteCommentR project_handle target comment_id = do
    user_id <- requireAuthId
    comment <- runDB $ get404 comment_id

    can_delete <- runDB $ canDeleteComment user_id (Entity comment_id comment)
    unless can_delete $
        permissionDenied "You can't delete that comment."

    runDB $ deleteComment comment_id

    addAlert "success" "comment deleted"
    redirect $ DiscussWikiR project_handle target

--------------------------------------------------------------------------------
-- /edit

getEditCommentR :: Text -> Text -> CommentId -> Handler Html
getEditCommentR project_handle target comment_id = do
    void requireAuth
    comment <- runDB $ get404 comment_id
    comment_widget <- makeCommentWidget
                        getMaxDepthZero
                        True
                        (commentEditFormWidget $ commentText comment)
                        project_handle
                        target
                        comment_id
    defaultLayout $(widgetFile "comment_wrapper")

postEditCommentR :: Text -> Text -> CommentId -> Handler Html
postEditCommentR project_handle target comment_id = do
    void $ checkCommentPage project_handle target comment_id
    ((result, _), _) <- runFormPost $ commentEditForm ""
    case result of
        FormSuccess new_text -> lookupPostParam "mode" >>= \case
            Just "post"    -> postEdit new_text
            Just "preview" -> previewEdit new_text
            m              -> error $ "Error: unrecognized mode (" ++ show m ++ ")"
        FormMissing -> error "Form missing."
        FormFailure msgs -> error $ "Error submitting form: " ++ T.unpack (T.intercalate "\n" msgs)
  where
    previewEdit :: Markdown -> Handler Html
    previewEdit new_text = do
        (form, _) <- generateFormPost $ commentEditForm new_text
        comment_widget <- previewWidget form "post" <$> makeCommentWidgetMod
                                                          mods
                                                          getMaxDepthZero
                                                          False
                                                          mempty
                                                          project_handle
                                                          target
                                                          comment_id
        defaultLayout $(widgetFile "comment_wrapper")
      where
        mods :: CommentMods
        mods = def
            { mod_comment = \c -> c { commentText = new_text }
            -- Since an edit removes a flagging, don't show the flagged markup in preview.
            , mod_flag_map = M.delete comment_id
            }

    postEdit :: Markdown -> Handler Html
    postEdit new_text = do
        user_id <- requireAuthId
        (_, _, comment) <- checkCommentPage project_handle target comment_id

        unless (canEditComment user_id comment) $
            permissionDenied "You can't edit that comment."

        editComment comment_id new_text

        addAlert "success" "comment edited"
        redirect $ DiscussCommentR project_handle target comment_id

--------------------------------------------------------------------------------
-- /flag

getFlagCommentR :: Text -> Text -> CommentId -> Handler Html
getFlagCommentR project_handle target comment_id = do
    void requireAuth
    comment_widget <- makeCommentWidget
                        getMaxDepthZero
                        True
                        widget
                        project_handle
                        target
                        comment_id
    defaultLayout $(widgetFile "comment_wrapper")
  where
    widget = do
        (form, enctype) <- handlerToWidget $ generateFormPost (flagCommentForm Nothing Nothing)
        [whamlet|
            <form method="POST" enctype=#{enctype}>
                <h4>Code of Conduct Violation(s):
                ^{form}
                <input .flag-submit type="submit" value="flag comment">
                <input type="hidden" name="mode" value="preview">
        |]

postFlagCommentR :: Text -> Text -> CommentId -> Handler Html
postFlagCommentR project_handle target comment_id = do
    user_id <- requireAuthId
    void $ checkCommentPage project_handle target comment_id
    ((result, _), _) <- runFormPost (flagCommentForm Nothing Nothing)
    case result of
        -- TODO(mitchell): Change the form to just return [FlagReason], not Maybe [FlagReason]
        FormSuccess (Nothing, _) -> flagFailure "Please check at least one Code of Conduct violation."
        FormSuccess (Just [], _) -> flagFailure "Please check at least one Code of Conduct violation."
        FormSuccess (Just reasons, message) -> lookupPostParam "mode" >>= \case
            Just "flag comment" -> postFlag user_id reasons message
            Just "preview"      -> previewFlag reasons message
            m                   -> error $ "Error: unrecognized mode (" ++ show m ++ ")"
        FormFailure errs -> flagFailure (T.intercalate ", " errs)
        _ -> flagFailure "Form missing."
  where
    postFlag :: UserId -> [FlagReason] -> Maybe Markdown -> Handler Html
    postFlag user_id reasons message = do
            permalink_route <- getUrlRender <*> pure (EditCommentR project_handle target comment_id)
            success <- runDB $ flagComment
                                 project_handle
                                 target
                                 comment_id
                                 permalink_route
                                 user_id
                                 reasons
                                 message
            if success
                then addAlert "success" "comment hidden and flagged for revision"
                else addAlert "danger" "error: another user flagged this just before you"
            redirect $ DiscussWikiR project_handle target

    previewFlag :: [FlagReason] -> Maybe Markdown -> Handler Html
    previewFlag reasons message = do
        (form, _) <- generateFormPost $ flagCommentForm (Just (Just reasons)) (Just message)

        let mods = def { mod_flag_map = M.insert comment_id (message, reasons) }
        unwrapped_comment_widget <-
            makeCommentWidgetMod
              mods
              getMaxDepthZero
              True
              mempty
              project_handle
              target
              comment_id

        let form_with_header =
                [whamlet|
                    <h4>Code of Conduct Violation(s):
                    ^{form}
                |]
            comment_widget = do
                previewWidget form_with_header "flag comment" unwrapped_comment_widget
                -- Duplicate .flag-submit markup from default-layout.cassius, so the
                -- pre-preview page (GET /flag) button looks the same as the preview
                -- page's. This is unfortunate.
                toWidget [cassius|
                    .preview-action-button[type=submit]
                        background : red
                        background-image : linear-gradient(#df6955, #a5022a)
                        border-color: #a5022a

                    .preview-action-button[type=submit]:hover, .flag-submit[type=submit]:focus, .flag-submit[type=submit]:active
                        background : dark-red
                        background-image : linear-gradient(#dd4f48, #d91700)
                |]
        defaultLayout $(widgetFile "comment_wrapper")

flagFailure :: Text -> Handler a
flagFailure msg = do
    addAlert "danger" msg
    Just route <- getCurrentRoute
    redirect route

--------------------------------------------------------------------------------
-- /moderate

getApproveWikiCommentR :: Text -> Text -> CommentId -> Handler Html
getApproveWikiCommentR project_handle target comment_id = do
    void $ sanityCheckApprove project_handle
    comment_widget <- makeCommentWidget
                        getMaxDepth
                        True
                        widget
                        project_handle
                        target
                        comment_id
    defaultLayout $(widgetFile "comment_wrapper")
  where
    widget = [whamlet|
        <form method="POST">
            <input type=submit value="approve post">
    |]

postApproveWikiCommentR :: Text -> Text -> CommentId -> Handler Html
postApproveWikiCommentR project_handle target comment_id = do
    void $ checkCommentPage project_handle target comment_id
    user_id <- sanityCheckApprove project_handle

    runDB $ approveComment user_id comment_id
    addAlert "success" "comment approved"
    redirect $ DiscussCommentR project_handle target comment_id

sanityCheckApprove :: Text -> Handler UserId
sanityCheckApprove project_handle = do
    user_id <- requireAuthId
    requireModerator "You must be a moderator to approve posts." project_handle user_id
    return user_id

--------------------------------------------------------------------------------
-- /close and /retract

getRetractWikiCommentR :: Text -> Text -> CommentId -> Handler Html
getRetractWikiCommentR project_handle target comment_id = do
    -- This function calls checkCommentPage twice: once in retractSanityCheck, and another
    -- time in makeCommentWidget. Maybe it should be taken out of makeCommentWidget,
    -- and the handlers should be in charge of calling it?
    retractSanityCheck project_handle target comment_id
    comment_widget <- makeCommentWidget
                        getMaxDepth
                        True
                        widget
                        project_handle
                        target
                        comment_id
    defaultLayout $(widgetFile "comment_wrapper")
  where
    widget :: Widget
    widget = do
        (form, enctype) <- handlerToWidget . generateFormPost $ retractedForm Nothing
        [whamlet|
            <form method="POST" enctype=#{enctype}>
                ^{form}
                <input type="submit" name="mode" value="preview">
        |]

retractSanityCheck :: Text -> Text -> CommentId -> Handler ()
retractSanityCheck project_handle target comment_id = do
    user_id <- requireAuthId
    (_, _, comment) <- checkCommentPage project_handle target comment_id
    when (commentUser comment /= user_id) $
        permissionDenied "You can only retract your own comments."

getCloseWikiCommentR :: Text -> Text -> CommentId -> Handler Html
getCloseWikiCommentR project_handle target comment_id = do
    closeSanityCheck project_handle target comment_id
    comment_widget <- makeCommentWidget
                        getMaxDepth
                        True
                        widget
                        project_handle
                        target
                        comment_id
    defaultLayout $(widgetFile "comment_wrapper")
  where
    widget :: Widget
    widget = do
        (form, enctype) <- handlerToWidget . generateFormPost $ closedForm Nothing
        [whamlet|
            <form method="POST" enctype=#{enctype}>
                ^{form}
                <input type="submit" name="mode" value="preview">
        |]

-- Same signature as retractSanityCheck, for use in postClosureWikiComment
closeSanityCheck :: Text -> Text -> CommentId -> Handler ()
closeSanityCheck _ _ _ = do
    user <- entityVal <$> requireAuth

    -- TODO: what should this be?
    -- Aaron says: I think we should allow established to mark as closed,
    -- but only *affiliated* OR the original poster should do so in one step,
    -- otherwise, the marking of closed should require *moderator* confirmation…
    -- We should also have a re-open function.
    -- There are now comments discussing these things on the site.
    unless (isEstablished user) $
        permissionDenied "You must be an established user to close a conversation."

postRetractWikiCommentR, postCloseWikiCommentR :: Text -> Text -> CommentId -> Handler Html
postRetractWikiCommentR = postClosureWikiComment retractSanityCheck retractedForm newRetractedCommentClosure "retract"
postCloseWikiCommentR   = postClosureWikiComment closeSanityCheck   closedForm    newClosedCommentClosure    "close"

-- | POST handler for either closing or retracting a comment, which are very similar.
postClosureWikiComment :: (Text -> Text -> CommentId -> Handler ())
                       -> (Maybe Markdown -> Form Markdown)
                       -> (UserId -> Markdown -> CommentId -> Handler CommentClosure)
                       -> Text
                       -> Text
                       -> Text
                       -> CommentId
                       -> Handler Html
postClosureWikiComment sanity_check make_closure_form make_new_comment_closure action project_handle target comment_id = do
    sanity_check project_handle target comment_id
    ((result, _), _) <- runFormPost $ make_closure_form Nothing
    case result of
        FormSuccess reason -> do
            user_id <- requireAuthId
            new_comment_closure <- make_new_comment_closure user_id reason comment_id
            lookupPostParam "mode" >>= \case
                Just "preview" -> do
                    (form, _) <- generateFormPost $ make_closure_form (Just reason)
                    comment_widget <- previewWidget form action <$>
                                          makeCommentWidgetMod
                                            (def { mod_closure_map = M.insert comment_id new_comment_closure })
                                            getMaxDepthZero
                                            False
                                            mempty -- TODO(mitchell): is this right?
                                            project_handle
                                            target
                                            comment_id
                    defaultLayout $(widgetFile "comment_wrapper")
                Just mode | mode == action -> do
                    runDB $ insert_ new_comment_closure
                    redirect $ DiscussCommentR project_handle target comment_id
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
    -- TODO (0): AVOID CYCLES

    (Entity project_id _, _, comment) <- checkCommentPage project_handle target comment_id

    user_id <- requireAuthId
    ok <- runDB $ isProjectModerator' user_id project_id
    unless ok $
        permissionDenied "You must be a moderator to rethread"

    ((result, _), _) <- runFormPost rethreadForm

    case result of
        FormSuccess (new_parent_url, reason) -> do
            app <- getYesod
            let splitPath  = drop 1 . T.splitOn "/"
                stripQuery = fst . T.break (== '?')
                stripRoot  = fromMaybe new_parent_url . T.stripPrefix (appRoot $ settings app)
                url        = splitPath $ stripQuery $ stripRoot new_parent_url

            (new_parent_id, new_discussion_id) <- case parseRoute (url, []) of
                Just (DiscussCommentR new_project_handle new_target new_parent_id) -> do
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

            new_parent_depth <- maybe (return $ -1) getCommentDepth404 new_parent_id
            old_parent_depth <- maybe (return $ -1) getCommentDepth404 old_parent_id

            let depth_offset = old_parent_depth - new_parent_depth

            mode <- lookupPostParam "mode"
            let action :: Text = "rethread"
            case mode of
                Just "preview" -> error "no preview for rethreads yet" -- TODO

                Just action' | action' == action -> do
                    now <- liftIO getCurrentTime

                    runDB $ do
                        descendants <- getCommentDescendantsIds comment_id

                        let comments = comment_id : descendants

                        rethread_id <- insert $ Rethread now user_id comment_id reason

                        new_comment_ids <- rethreadComments rethread_id depth_offset new_parent_id new_discussion_id comments

                        delete $
                            from $ \ca ->
                            where_ $ ca ^. CommentAncestorComment `in_` valList comments

                        forM_ new_comment_ids $ \ new_comment_id -> do
                            insertSelect $
                                from $ \ (c `InnerJoin` ca) -> do
                                on_ $ c ^. CommentParent ==. just (ca ^. CommentAncestorComment)
                                where_ $ c ^. CommentId ==. val new_comment_id
                                return $ CommentAncestor <# val new_comment_id <&> (ca ^. CommentAncestorAncestor)

                            [Value maybe_new_parent_id] <-
                                select $
                                    from $ \ c -> do
                                    where_ $ c ^. CommentId ==. val new_comment_id
                                    return (c ^. CommentParent)

                            maybe (return ()) (insert_ . CommentAncestor new_comment_id) maybe_new_parent_id

                        when (new_discussion_id /= commentDiscussion comment) $
                            update $ \c -> do
                                where_ $ c ^. CommentId `in_` valList descendants
                                set c [ CommentDiscussion =. val new_discussion_id ]

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
-- /tags/*

getCommentTagsR :: Text -> Text -> CommentId -> Handler Html
getCommentTagsR project_handle target comment_id = do
    void $ checkCommentPage project_handle target comment_id

    comment_tags <- map entityVal <$> runDB (getCommentTags comment_id)

    let tag_ids = S.toList . S.fromList $ map commentTagTag comment_tags
    tag_map <- fmap entitiesMap . runDB $
        select $
            from $ \tag -> do
            where_ (tag ^. TagId `in_` valList tag_ids)
            return tag

    renderTags =<< buildAnnotatedTags tag_map (CommentTagR project_handle target comment_id) comment_tags
  where
    renderTags tags = defaultLayout $(widgetFile "tags")

getCommentTagR :: Text -> Text -> CommentId -> TagId -> Handler Html
getCommentTagR project_handle target comment_id tag_id = do
    void $  checkCommentPage project_handle target comment_id

    comment_tags <- map entityVal <$> runDB (
        select $
            from $ \comment_tag -> do
            where_ (comment_tag ^. CommentTagComment ==. val comment_id &&.
                    comment_tag ^. CommentTagTag ==. val tag_id)
            return comment_tag)

    let tag_ids = S.toList . S.fromList $ map commentTagTag comment_tags
    tag_map <- fmap entitiesMap $ runDB $ select $ from $ \ tag -> do
        where_ $ tag ^. TagId `in_` valList tag_ids
        return tag

    annotated_tags <- buildAnnotatedTags tag_map (CommentTagR project_handle target comment_id) comment_tags

    case annotated_tags of
        [] -> error "That tag has not been applied to this comment."
        [tag] -> renderTag tag
        _ -> error "This should never happen."
  where
    renderTag (AnnotatedTag tag _ _ user_votes) = do
        let tag_name = tagName $ entityVal tag
        defaultLayout $(widgetFile "tag")

postCommentTagR :: Text -> Text -> CommentId -> TagId -> Handler Html
postCommentTagR project_handle target comment_id tag_id = do
    user_id <- requireAuthId
    void $  checkCommentPage project_handle target comment_id

    direction <- lookupPostParam "direction"

    let delta = case T.unpack <$> direction of
            Just "+" -> 1
            Just "-" -> -1
            Just "\215" -> -1
            Nothing -> error "direction unset"
            Just str -> error $ "unrecognized direction: " ++ str

    runDB $ do
        maybe_comment_tag_entity <- getBy $ UniqueCommentTag comment_id tag_id user_id
        case maybe_comment_tag_entity of
            Nothing -> void $ insert $ CommentTag comment_id tag_id user_id delta
            Just (Entity comment_tag_id comment_tag) -> case commentTagCount comment_tag + delta of
                0 -> delete $ from $ \ ct -> where_ $ ct ^. CommentTagId ==. val comment_tag_id
                x -> void $ update $ \ ct -> do
                    set ct [ CommentTagCount =. val x ]
                    where_ $ ct ^. CommentTagId ==. val comment_tag_id

    setUltDestReferer
    redirectUltDest $ CommentTagR project_handle target comment_id tag_id

getNewCommentTagR :: Text -> Text -> CommentId -> Handler Html
getNewCommentTagR project_handle target comment_id = do
    void . runDB $ get404 comment_id

    user <- entityVal <$> requireAuth

    unless (isEstablished user)
        (permissionDenied "You must be an established user to add tags")

    (Entity project_id _, _, _) <- checkCommentPage project_handle target comment_id

    comment_tags <- fmap (map entityVal) $ runDB $ select $ from $ \ comment_tag -> do
        where_ $ comment_tag ^. CommentTagComment ==. val comment_id
        return comment_tag

    tag_map <- fmap entitiesMap $ runDB $ select $ from $ \ tag -> do
        where_ $ tag ^. TagId `in_` valList (S.toList $ S.fromList $ map commentTagTag comment_tags)
        return tag

    tags <- annotateCommentTags tag_map project_handle target comment_id comment_tags

    (project_tags, other_tags) <- runDB $ getProjectTagList project_id

    let filter_tags = filter (\(Entity t _) -> not $ M.member t tag_map)
    (apply_form, _) <- generateFormPost $ newCommentTagForm (filter_tags project_tags) (filter_tags other_tags)
    (create_form, _) <- generateFormPost $ createCommentTagForm

    defaultLayout $(widgetFile "new_comment_tag")

postCreateNewCommentTagR, postApplyNewCommentTagR :: Text -> Text -> CommentId -> Handler Html
postCreateNewCommentTagR = postNewCommentTagR True
postApplyNewCommentTagR  = postNewCommentTagR False

postNewCommentTagR :: Bool -> Text -> Text -> CommentId -> Handler Html
postNewCommentTagR create_tag project_handle target comment_id = do
    Entity user_id user <- requireAuth

    unless (isEstablished user)
        (permissionDenied "You must be an established user to add tags")

    (Entity project_id _, _, _) <- checkCommentPage project_handle target comment_id

    let formFailure es = error $ T.unpack $ "form submission failed: " <> T.intercalate "; " es

    if create_tag
        then do
            ((result_create, _), _) <- runFormPost $ createCommentTagForm
            case result_create of
                FormSuccess (tag_name) -> do
                    msuccess <- runDB $ do
                        maybe_tag <- getBy $ UniqueTag tag_name
                        case maybe_tag of
                            Nothing -> do
                                tag_id <- insert $ Tag tag_name
                                void $ insert $ CommentTag comment_id tag_id user_id 1
                            Just _ -> do
                                return ()
                        return maybe_tag
                    if (isJust $ msuccess) then do
                        addAlert "danger" "that tag already exists"
                        redirectUltDest $ NewCommentTagR project_handle target comment_id
                        else do
                            redirectUltDest $ DiscussCommentR project_handle target comment_id
                FormMissing -> error "form missing"
                FormFailure es -> formFailure es
        else do
            comment_tags <- fmap (map entityVal) $ runDB $ select $ from $ \ comment_tag -> do
                where_ $ comment_tag ^. CommentTagComment ==. val comment_id
                return comment_tag

            tag_map <- fmap entitiesMap $ runDB $ select $ from $ \ tag -> do
                where_ $ tag ^. TagId `in_` valList (S.toList $ S.fromList $ map commentTagTag comment_tags)
                return tag
            let filter_tags = filter (\(Entity t _) -> not $ M.member t tag_map)
            (project_tags, other_tags) <- runDB $ getProjectTagList project_id
            ((result_apply, _), _) <- runFormPost $ newCommentTagForm (filter_tags project_tags) (filter_tags other_tags)
            case result_apply of
                FormSuccess (mproject_tag_ids, mother_tag_ids) -> do
                    let project_tag_ids = fromMaybe [] mproject_tag_ids
                    let other_tag_ids = fromMaybe [] mother_tag_ids
                    runDB $ do
                        let tag_ids = project_tag_ids <> other_tag_ids
                        valid_tags <- select $ from $ \tag -> do
                            where_ ( tag ^. TagId `in_` valList tag_ids )
                            return tag
                        if (null valid_tags)
                            then
                                permissionDenied "error: invalid tag id"
                            else
                                void $ insertMany $ fmap (\(Entity tag_id _) -> CommentTag comment_id tag_id user_id 1) valid_tags
                        -- case maybe_tag of
                        --    Nothing -> permissionDenied "tag does not exist"
                        --    Just _ -> void $ insert $ CommentTag comment_id tag_id user_id 1
                    redirectUltDest $ DiscussCommentR project_handle target comment_id
                FormMissing -> error "form missing"
                FormFailure es -> formFailure (es <> [T.pack " apply"])

-- Some additional helpers. These sort of belong in View, but that would cause a
-- circular dependency.

-- | Data type used in makeCommentWidgetMod, containing modifications to comment-action-related
-- data structures.
data CommentMods = CommentMods
    { mod_comment          :: Comment          -> Comment
    , mod_earlier_closures :: [CommentClosure] -> [CommentClosure]
    , mod_user_map         :: UserMap          -> UserMap
    , mod_closure_map      :: ClosureMap       -> ClosureMap
    , mod_ticket_map       :: TicketMap        -> TicketMap
    , mod_flag_map         :: FlagMap          -> FlagMap
    , mod_tag_map          :: TagMap           -> TagMap
    }

instance Default CommentMods where
    def = CommentMods id id id id id id id

-- | Helper method to create a Widget for a comment action (/, /reply, /moderate, etc).
-- Returns a Widget from a Handler (rather than just calling defaultLayout) so that the widget
-- can be put in a preview (for some POST handlers).
makeCommentWidget :: Handler Int    -- ^ Max depth getter.
                  -> Bool           -- ^ Show actions?
                  -> Widget         -- ^ Widget to display under root comment.
                  -> Text           -- ^ Project handle.
                  -> Text           -- ^ Target.
                  -> CommentId      -- ^ Root comment id.
                  -> Handler Widget
makeCommentWidget = makeCommentWidgetMod def

-- | Like @makeCommentWidget@, but includes modifications to the datastructures grabbed from
-- the database. This is used for showing previews of comment trees, where changes are not
-- saved yet.
makeCommentWidgetMod :: CommentMods    -- ^ Comment structure modifications.
                     -> Handler Int    -- ^ Max depth getter.
                     -> Bool           -- ^ Is preview_
                     -> Widget         -- ^ Widget to display under root comment.
                     -> Text           -- ^ Project handle.
                     -> Text           -- ^ Target.
                     -> CommentId      -- ^ Root comment id.
                     -> Handler Widget
makeCommentWidgetMod CommentMods{..} get_max_depth show_actions form_under_root_comment project_handle target comment_id = do
    (Entity project_id _, _, root) <-
        -- TODO(mitchell)
        -- (_3 %~ mod_comment) <$> checkCommentPage project_handle target comment_id
        (\(a,b,c) -> (a,b,mod_comment c)) <$> checkCommentPage project_handle target comment_id

    mviewer_id <- maybeAuthId
    (rest, user_map, earlier_closures, closure_map, ticket_map, flag_map, tag_map) <- runDB $ do
        rest <- getCommentDescendants mviewer_id project_id comment_id

        let all_comments    = (Entity comment_id root):rest
            all_comment_ids = map entityKey all_comments

        earlier_closures <- getAncestorClosures comment_id
        user_map         <- entitiesMap <$> getUsersIn (S.toList $ getCommentsUsers all_comments)
        closure_map      <- makeClosureMap all_comment_ids
        ticket_map       <- makeTicketMap  all_comment_ids
        flag_map         <- makeFlagMap    all_comment_ids
        tag_map          <- entitiesMap <$> getAllTags

        return (rest, user_map, earlier_closures, closure_map, ticket_map, flag_map, tag_map)

    user_map_with_viewer <- (maybe id (\(Entity viewer_id viewer) -> M.insert viewer_id viewer))
        <$> maybeAuth
        <*> pure user_map

    max_depth <- get_max_depth
    return $
        commentTreeWidget
            form_under_root_comment
            (sortTreeBy orderingNewestFirst $ buildCommentTree (Entity comment_id root, rest))
            (mod_earlier_closures earlier_closures)
            (mod_user_map         user_map_with_viewer)
            (mod_closure_map      closure_map)
            (mod_ticket_map       ticket_map)
            (mod_flag_map         flag_map)
            (mod_tag_map          tag_map)
            project_handle
            target
            show_actions
            max_depth
            0

--------------------------------------------------------------------------------
-- DEPRECATED

-- This is just because we used to have "/comment/#" with that longer URL,
-- and this keeps any permalinks from breaking
getOldDiscussCommentR :: Text -> Text -> CommentId -> Handler Html
getOldDiscussCommentR project_handle target comment_id = redirect $ DiscussCommentR project_handle target comment_id
