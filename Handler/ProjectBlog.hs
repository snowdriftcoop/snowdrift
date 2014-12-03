module Handler.ProjectBlog where

import Import

import Handler.Comment
import Handler.Discussion
import Handler.Utils
import Model.Blog
import Model.Comment
import Model.Comment.ActionPermissions
import Model.Comment.HandlerInfo
import Model.Comment.Mods
import Model.Comment.Sql
import Model.Discussion
import Model.Markdown
import Model.User
import View.Comment
import View.Project

import Widgets.Preview
import Widgets.Time

import Yesod.Markdown

import           Data.Default
import qualified Data.Text      as T
import           Data.Tree      (Forest, Tree)
import qualified Data.Tree      as Tree

import           Text.Cassius   (cassiusFile)


-- | Sanity check for Project Comment pages. Redirects if the comment was rethreaded.
-- 404's if the comment doesn't exist. 403 if permission denied.
checkComment :: Text -> Text -> CommentId -> Handler (Maybe (Entity User), Entity Project, Comment)
checkComment project_handle post_name comment_id = do
    muser <- maybeAuth
    (project, comment) <- checkComment' (entityKey <$> muser) project_handle post_name comment_id
    return (muser, project, comment)

-- | Like checkComment, but authentication is required.
checkCommentRequireAuth :: Text -> Text -> CommentId -> Handler (Entity User, Entity Project, Comment)
checkCommentRequireAuth project_handle post_name comment_id = do
    user@(Entity user_id _) <- requireAuth
    (project, comment) <- checkComment' (Just user_id) project_handle post_name comment_id
    return (user, project, comment)

-- | Abstract checkComment and checkCommentRequireAuth. You shouldn't use this function directly.
checkComment' :: Maybe UserId -> Text -> Text -> CommentId -> Handler (Entity Project, Comment)
checkComment' muser_id project_handle post_name comment_id = do
    redirectIfRethreaded comment_id

    (project, blog_post, ecomment) <- runYDB $ do
        liftIO $ appendFile "log" $ unwords [ "getting project with handle", show project_handle ] ++ "\n"
        project@(Entity project_id _)   <- getBy404 $ UniqueProjectHandle project_handle

        liftIO $ appendFile "log" $ unwords [ "getting post for project", show project_id, "with handle", show post_name ] ++ "\n"
        Entity _ blog_post              <- getBy404 $ UniqueBlogPost project_id post_name

        let has_permission = exprCommentProjectPermissionFilter muser_id (val project_id)

        ecomment <- fetchCommentDB comment_id has_permission
        return (project, blog_post, ecomment)

    case ecomment of
        Left CommentNotFound         -> do
            liftIO $ appendFile "log" "comment not found"
            notFound

        Left CommentPermissionDenied -> permissionDenied "You don't have permission to view this comment."
        Right comment                ->
            if commentDiscussion comment /= blogPostDiscussion blog_post
                then do
                    liftIO $ appendFile "log" $ unwords
                        [ "comment discussion"
                        , show $ commentDiscussion comment
                        , "does not match blog post discussion"
                        , show $ blogPostDiscussion blog_post
                        ]

                    notFound

                else return (project, comment)


checkBlogPostCommentActionPermission
        :: (CommentActionPermissions -> Bool)
        -> Entity User
        -> Text
        -> Entity Comment
        -> Handler ()
checkBlogPostCommentActionPermission
        can_perform_action
        user
        project_handle
        comment@(Entity comment_id _) = do

    action_permissions <-
        lookupErr "checkBlogPostCommentActionPermission: comment id not found in map" comment_id
            <$> makeProjectCommentActionPermissionsMap (Just user) project_handle def [comment]

    unless (can_perform_action action_permissions) $ permissionDenied "You don't have permission to perform this action."

makeBlogPostCommentForestWidget
        :: Maybe (Entity User)
        -> ProjectId
        -> Text
        -> Text
        -> [Entity Comment]
        -> CommentMods
        -> Handler MaxDepth
        -> Bool
        -> Widget
        -> Handler (Widget, Forest (Entity Comment))
makeBlogPostCommentForestWidget
        muser
        project_id
        project_handle
        post_name
        comments
        comment_mods
        get_max_depth
        is_preview
        widget_under_root_comment = do
    makeCommentForestWidget
      (projectBlogCommentHandlerInfo muser project_id project_handle post_name)
      comments
      muser
      comment_mods
      get_max_depth
      is_preview
      widget_under_root_comment

makeBlogPostCommentTreeWidget
        :: Maybe (Entity User)
        -> ProjectId
        -> Text
        -> Text
        -> Entity Comment
        -> CommentMods
        -> Handler MaxDepth
        -> Bool
        -> Widget
        -> Handler (Widget, Tree (Entity Comment))
makeBlogPostCommentTreeWidget muser project_id project_handle d comment mods max_depth is_preview widget_under_root_comment = do
    (widget, [tree]) <- makeBlogPostCommentForestWidget muser project_id project_handle d [comment] mods max_depth is_preview widget_under_root_comment
    return (widget, tree)

makeBlogPostCommentActionWidget
        :: MakeCommentActionWidget
        -> Text
        -> Text
        -> CommentId
        -> CommentMods
        -> Handler MaxDepth
        -> Handler (Widget, Tree (Entity Comment))
makeBlogPostCommentActionWidget make_comment_action_widget project_handle post_name comment_id mods get_max_depth = do
    (user, Entity project_id _, comment) <- checkCommentRequireAuth project_handle post_name comment_id
    make_comment_action_widget
        (Entity comment_id comment)
        user
        (projectCommentHandlerInfo (Just user) project_id project_handle)
        mods
        get_max_depth
        False

projectBlogDiscussionPage :: Text -> Text -> Widget -> Widget
projectBlogDiscussionPage project_handle post_name widget = do
    $(widgetFile "project_blog_discussion_wrapper")
    toWidget $(cassiusFile "templates/comment.cassius")

-- | Require any of the given Roles, failing with permissionDenied if none are satisfied.
requireRolesAny :: [Role] -> Text -> Text -> Handler (UserId, Entity Project)
requireRolesAny roles project_handle err_msg = do
    user_id <- requireAuthId

    (project, ok) <- runYDB $ do
        project@(Entity project_id _) <- getBy404 (UniqueProjectHandle project_handle)

        ok <- userHasRolesAnyDB roles user_id project_id

        return (project, ok)

    unless ok $
        permissionDenied err_msg

    return (user_id, project)

--------------------------------------------------------------------------------
-- /p/#Text/blog

getProjectBlogR :: Text -> Handler Html
getProjectBlogR project_handle = do
    maybe_from <- fmap (Key . PersistInt64 . read . T.unpack) <$> lookupGetParam "from"
    post_count <- fromMaybe 10 <$> fmap (read . T.unpack) <$> lookupGetParam "from"
    Entity project_id project <- runYDB $ getBy404 $ UniqueProjectHandle project_handle

    let apply_offset blog = maybe id (\ from_blog rest -> blog ^. BlogPostId >=. val from_blog &&. rest) maybe_from

    (posts, next) <- fmap (splitAt post_count) $ runDB $
        select $
        from $ \blog -> do
        where_ $ apply_offset blog $ blog ^. BlogPostProject ==. val project_id
        orderBy [ desc $ blog ^. BlogPostTs, desc $ blog ^. BlogPostId ]
        limit (fromIntegral post_count + 1)
        return blog

    renderRouteParams <- getUrlRenderParams

    let nextRoute next_id = renderRouteParams (ProjectBlogR project_handle) [("from", toPathPiece next_id)]
        discussion = DiscussionOnProject $ Entity project_id project

    defaultLayout $ do
        setTitle . toHtml $ projectName project <> " Blog | Snowdrift.coop"

        $(widgetFile "project_blog")


--------------------------------------------------------------------------------
-- /p/#Text/blog/!new

getNewBlogPostR :: Text -> Handler Html
getNewBlogPostR project_handle = do
    (_, Entity _ project) <- requireRolesAny [Admin, TeamMember] project_handle "You do not have permission to post to this project's blog."

    (blog_form, _) <- generateFormPost $ projectBlogForm Nothing

    defaultLayout $ do
        setTitle . toHtml $ "Post To " <> projectName project <> " Blog | Snowdrift.coop"

        $(widgetFile "new_blog_post")


postNewBlogPostR :: Text -> Handler Html
postNewBlogPostR project_handle = do
    (viewer_id, Entity project_id _) <-
        requireRolesAny [Admin, TeamMember] project_handle "You do not have permission to post to this project's blog."

    now <- liftIO getCurrentTime

    ((result, _), _) <- runFormPost $ projectBlogForm Nothing

    case result of
        FormSuccess (title, handle, Markdown content) -> do
            lookupPostMode >>= \case
                Just PostMode -> do
                    void $ runSDB $ postBlogPostDB title handle viewer_id project_id (Markdown content)
                    alertSuccess "posted"
                    redirect $ ProjectBlogR project_handle

                _ -> do

                    let (top_content', bottom_content') = break (== "***") $ T.lines content
                        top_content = T.unlines top_content'
                        bottom_content = if bottom_content' == [] then Nothing else Just (Markdown $ T.unlines bottom_content')
                        blog_post = BlogPost now title handle viewer_id project_id (Key $ PersistInt64 0) (Markdown top_content) bottom_content

                    (form, _) <- generateFormPost $ projectBlogForm $ Just (title, handle, Markdown content)

                    defaultLayout $ previewWidget form "post" $ renderBlogPost project_handle blog_post

        x -> do
            alertDanger $ T.pack $ show x
            redirect $ NewBlogPostR project_handle


--------------------------------------------------------------------------------
-- /p/#Text/blog/#Text

getBlogPostR :: Text -> Text -> Handler Html
getBlogPostR project_handle blog_post_handle = do
    (project, blog_post) <- runYDB $ do
        Entity project_id project <- getBy404 $ UniqueProjectHandle project_handle
        Entity _ blog_post <- getBy404 $ UniqueBlogPost project_id blog_post_handle

        return (project, blog_post)

    defaultLayout $ do
        setTitle . toHtml $ projectName project <> " Blog - " <> blogPostTitle blog_post <> " | Snowdrift.coop"

        renderBlogPost project_handle blog_post

--------------------------------------------------------------------------------
-- /p/#Text/blog/#Text/c/#CommentId

getBlogPostCommentR :: Text -> Text -> CommentId -> Handler Html
getBlogPostCommentR project_handle post_name comment_id = do
    (muser, Entity project_id _, comment) <- checkComment project_handle post_name comment_id
    (widget, comment_tree) <-
        makeBlogPostCommentTreeWidget
            muser
            project_id
            project_handle
            post_name
            (Entity comment_id comment)
            def
            getMaxDepth
            False
            mempty

    case muser of
        Nothing -> return ()
        Just (Entity user_id _) ->
            runDB (userMaybeViewProjectCommentsDB user_id project_id (map entityKey (Tree.flatten comment_tree)))

    defaultLayout $ projectBlogDiscussionPage project_handle post_name widget

--------------------------------------------------------------------------------
-- /p/#Text/blog/#Text/c/#CommentId/approve

getApproveBlogPostCommentR :: Text -> Text -> CommentId -> Handler Html
getApproveBlogPostCommentR project_handle post_name comment_id = do
    (widget, _) <- makeBlogPostCommentActionWidget makeApproveCommentWidget project_handle post_name comment_id def getMaxDepth

    defaultLayout $ projectBlogDiscussionPage project_handle post_name widget

postApproveBlogPostCommentR :: Text -> Text -> CommentId -> Handler Html
postApproveBlogPostCommentR project_handle post_name comment_id = do
    (user@(Entity user_id _), _, comment) <- checkCommentRequireAuth project_handle post_name comment_id
    checkBlogPostCommentActionPermission can_approve user project_handle (Entity comment_id comment)

    postApproveComment user_id comment_id comment
    redirect (BlogPostCommentR project_handle post_name comment_id)

--------------------------------------------------------------------------------
-- /p/#Text/blog/#Text/c/#CommentId/claim

getClaimBlogPostCommentR :: Text -> Text -> CommentId -> Handler Html
getClaimBlogPostCommentR project_handle post_name comment_id = do
    (widget, _) <- makeBlogPostCommentActionWidget makeClaimCommentWidget project_handle post_name comment_id def getMaxDepth

    defaultLayout $ projectBlogDiscussionPage project_handle post_name widget

postClaimBlogPostCommentR :: Text -> Text -> CommentId -> Handler Html
postClaimBlogPostCommentR project_handle post_name comment_id = do
    (user, (Entity project_id _), comment) <- checkCommentRequireAuth project_handle post_name comment_id

    checkBlogPostCommentActionPermission can_claim user project_handle (Entity comment_id comment)

    postClaimComment
      user
      comment_id
      comment
      (projectCommentHandlerInfo (Just user) project_id project_handle)
      >>= \case
        Nothing -> redirect (BlogPostCommentR project_handle post_name comment_id)
        Just (widget, form) -> defaultLayout $ previewWidget form "claim" $ projectBlogDiscussionPage project_handle post_name widget

--------------------------------------------------------------------------------
-- /p/#Text/blog/#Text/c/#CommentId/close

getCloseBlogPostCommentR :: Text -> Text -> CommentId -> Handler Html
getCloseBlogPostCommentR project_handle post_name comment_id = do
    (widget, _) <-
        makeBlogPostCommentActionWidget
            makeCloseCommentWidget
            project_handle
            post_name
            comment_id
            def
            getMaxDepth

    defaultLayout $ projectBlogDiscussionPage project_handle post_name widget


postCloseBlogPostCommentR :: Text -> Text -> CommentId -> Handler Html
postCloseBlogPostCommentR project_handle post_name comment_id = do
    (user, (Entity project_id _), comment) <- checkCommentRequireAuth project_handle post_name comment_id
    checkBlogPostCommentActionPermission can_close user project_handle (Entity comment_id comment)

    postCloseComment
      user
      comment_id
      comment
      (projectCommentHandlerInfo (Just user) project_id project_handle)
      >>= \case
        Nothing -> redirect (BlogPostCommentR project_handle post_name comment_id)
        Just (widget, form) -> defaultLayout $ previewWidget form "close" $ projectBlogDiscussionPage project_handle post_name widget

--------------------------------------------------------------------------------
-- /p/#Text/blog/#Text/c/#CommentId/delete

getDeleteBlogPostCommentR :: Text -> Text -> CommentId -> Handler Html
getDeleteBlogPostCommentR project_handle post_name comment_id = do
    (widget, _) <- makeBlogPostCommentActionWidget makeDeleteCommentWidget project_handle post_name comment_id def getMaxDepth

    defaultLayout $ projectBlogDiscussionPage project_handle post_name widget

postDeleteBlogPostCommentR :: Text -> Text -> CommentId -> Handler Html
postDeleteBlogPostCommentR project_handle post_name comment_id = do
    (user, _, comment) <- checkCommentRequireAuth project_handle post_name comment_id
    checkBlogPostCommentActionPermission can_delete user project_handle (Entity comment_id comment)

    was_deleted <- postDeleteComment comment_id
    if was_deleted
        then redirect $ BlogPostDiscussionR project_handle post_name
        else redirect $ BlogPostCommentR project_handle post_name comment_id

--------------------------------------------------------------------------------
-- /p/#Text/blog/#Text/c/#CommentId/edit

getEditBlogPostCommentR :: Text -> Text -> CommentId -> Handler Html
getEditBlogPostCommentR project_handle post_name comment_id = do
    (widget, _) <- makeBlogPostCommentActionWidget makeEditCommentWidget project_handle post_name comment_id def getMaxDepth

    defaultLayout $ projectBlogDiscussionPage project_handle post_name widget

postEditBlogPostCommentR :: Text -> Text -> CommentId -> Handler Html
postEditBlogPostCommentR project_handle post_name comment_id = do
    (user, Entity project_id _, comment) <- checkCommentRequireAuth project_handle post_name comment_id
    checkBlogPostCommentActionPermission can_edit user project_handle (Entity comment_id comment)

    postEditComment
      user
      (Entity comment_id comment)
      (projectCommentHandlerInfo (Just user) project_id project_handle)
      >>= \case
        Nothing -> redirect $ BlogPostCommentR project_handle post_name comment_id         -- Edit made.
        Just (widget, form) -> defaultLayout $ previewWidget form "post" $ projectBlogDiscussionPage project_handle post_name widget

--------------------------------------------------------------------------------
-- /p/#Text/blog/#Text/c/#CommentId/flag

getFlagBlogPostCommentR :: Text -> Text -> CommentId -> Handler Html
getFlagBlogPostCommentR project_handle post_name comment_id = do
    (widget, _) <- makeBlogPostCommentActionWidget makeFlagCommentWidget project_handle post_name comment_id def getMaxDepth

    defaultLayout $ projectBlogDiscussionPage project_handle post_name widget

postFlagBlogPostCommentR :: Text -> Text -> CommentId -> Handler Html
postFlagBlogPostCommentR project_handle post_name comment_id = do
    (user, Entity project_id _, comment) <- checkCommentRequireAuth project_handle post_name comment_id
    checkBlogPostCommentActionPermission can_flag user project_handle (Entity comment_id comment)

    postFlagComment
      user
      (Entity comment_id comment)
      (projectCommentHandlerInfo (Just user) project_id project_handle)
      >>= \case
        Nothing -> redirect $ BlogPostDiscussionR post_name project_handle
        Just (widget, form) -> defaultLayout $ previewWidget form "flag" $ projectBlogDiscussionPage project_handle post_name widget


--------------------------------------------------------------------------------
-- /p/#Text/blog/#Text/c/#CommentId/reply

getReplyBlogPostCommentR :: Text -> Text -> CommentId -> Handler Html
getReplyBlogPostCommentR project_handle post_name parent_id = do
    (widget, _) <- makeBlogPostCommentActionWidget makeReplyCommentWidget project_handle post_name parent_id def getMaxDepth

    defaultLayout $ projectBlogDiscussionPage project_handle post_name widget


postReplyBlogPostCommentR :: Text -> Text -> CommentId -> Handler Html
postReplyBlogPostCommentR project_handle post_name parent_id = do
    (user, Entity project_id _, parent) <- checkCommentRequireAuth project_handle post_name parent_id
    Entity _ BlogPost{..} <- runYDB $ getBy404 $ UniqueBlogPost project_id post_name
    checkBlogPostCommentActionPermission can_reply user project_handle (Entity parent_id parent)

    postNewComment (Just parent_id) user blogPostDiscussion (makeProjectCommentActionPermissionsMap (Just user) project_handle def) >>= \case
        Left _ -> redirect $ BlogPostCommentR project_handle post_name parent_id
        Right (widget, form) -> defaultLayout $ previewWidget form "post" $ projectBlogDiscussionPage project_handle post_name widget

--------------------------------------------------------------------------------
-- /p/#Text/blog/#Text/c/#CommentId/rethread

getRethreadBlogPostCommentR :: Text -> Text -> CommentId -> Handler Html
getRethreadBlogPostCommentR project_handle post_name comment_id = do
    (widget, _) <- makeBlogPostCommentActionWidget makeRethreadCommentWidget project_handle post_name comment_id def getMaxDepth

    defaultLayout $ projectBlogDiscussionPage project_handle post_name widget

postRethreadBlogPostCommentR :: Text -> Text -> CommentId -> Handler Html
postRethreadBlogPostCommentR project_handle post_name comment_id = do
    (user@(Entity user_id _), _, comment) <- checkCommentRequireAuth project_handle post_name comment_id
    checkBlogPostCommentActionPermission can_rethread user project_handle (Entity comment_id comment)
    postRethreadComment user_id comment_id comment

--------------------------------------------------------------------------------
-- /p/#Text/blog/#Text/c/#CommentId/retract

getRetractBlogPostCommentR :: Text -> Text -> CommentId -> Handler Html
getRetractBlogPostCommentR project_handle post_name comment_id = do
    (widget, _) <- makeBlogPostCommentActionWidget makeRetractCommentWidget project_handle post_name comment_id def getMaxDepth

    defaultLayout $ projectBlogDiscussionPage project_handle post_name widget

postRetractBlogPostCommentR :: Text -> Text -> CommentId -> Handler Html
postRetractBlogPostCommentR project_handle post_name comment_id = do
    (user, Entity project_id _, comment) <- checkCommentRequireAuth project_handle post_name comment_id
    checkBlogPostCommentActionPermission can_retract user project_handle (Entity comment_id comment)

    postRetractComment
      user
      comment_id
      comment
      (projectCommentHandlerInfo (Just user) project_id project_handle)
      >>= \case
        Nothing -> redirect $ BlogPostCommentR project_handle post_name comment_id
        Just (widget, form) -> defaultLayout $ previewWidget form "retract" $ projectBlogDiscussionPage project_handle post_name widget

--------------------------------------------------------------------------------
-- /p/#Text/blog/#Text/c/#CommentId/tags

getBlogPostCommentTagsR :: Text -> Text -> CommentId -> Handler Html
getBlogPostCommentTagsR _ _ = getCommentTags

--------------------------------------------------------------------------------
-- /p/#Text/blog/#Text/c/#CommentId/tag/#TagId

getBlogPostCommentTagR :: Text -> Text -> CommentId -> TagId -> Handler Html
getBlogPostCommentTagR _ _ = getCommentTagR

postBlogPostCommentTagR :: Text -> Text -> CommentId -> TagId -> Handler ()
postBlogPostCommentTagR _ _ = postCommentTagR

--------------------------------------------------------------------------------
-- /p/#Text/blog/#Text/c/#CommentId/tag/apply, /p/#Text/blog/c/#CommentId/tag/create

postBlogPostCommentApplyTagR, postBlogPostCommentCreateTagR:: Text -> Text -> CommentId -> Handler Html
postBlogPostCommentApplyTagR project_handle _ comment_id = applyOrCreate postCommentApplyTag project_handle comment_id
postBlogPostCommentCreateTagR project_handle _ comment_id = applyOrCreate postCommentCreateTag project_handle comment_id

applyOrCreate :: (CommentId -> Handler ()) -> Text -> CommentId -> Handler Html
applyOrCreate action project_handle comment_id = do
    action comment_id
    redirect (ProjectCommentR project_handle comment_id)

--------------------------------------------------------------------------------
-- /p/#Text/blog/#Text/c/#CommentId/tag/new

getBlogPostCommentAddTagR :: Text -> Text -> CommentId -> Handler Html
getBlogPostCommentAddTagR project_handle post_name comment_id = do
    (user@(Entity user_id _), Entity project_id _, comment) <- checkCommentRequireAuth project_handle post_name comment_id
    checkBlogPostCommentActionPermission can_add_tag user project_handle (Entity comment_id comment)
    getProjectCommentAddTag comment_id project_id user_id

--------------------------------------------------------------------------------
-- /p/#Text/blog/#Text/c/#CommentId/unclaim

getUnclaimBlogPostCommentR :: Text -> Text -> CommentId -> Handler Html
getUnclaimBlogPostCommentR project_handle post_name comment_id = do
    (widget, _) <- makeBlogPostCommentActionWidget makeUnclaimCommentWidget project_handle post_name comment_id def getMaxDepth

    defaultLayout $ projectBlogDiscussionPage project_handle post_name widget

postUnclaimBlogPostCommentR :: Text -> Text -> CommentId -> Handler Html
postUnclaimBlogPostCommentR project_handle post_name comment_id = do
    (user, (Entity project_id _), comment) <- checkCommentRequireAuth project_handle post_name comment_id
    checkBlogPostCommentActionPermission can_unclaim user project_handle (Entity comment_id comment)

    postUnclaimComment
      user
      comment_id
      comment
      (projectCommentHandlerInfo (Just user) project_id project_handle)
      >>= \case
        Nothing -> redirect $ BlogPostCommentR project_handle post_name comment_id
        Just (widget, form) -> defaultLayout $ previewWidget form "unclaim" $ projectBlogDiscussionPage project_handle post_name widget

--------------------------------------------------------------------------------
-- /p/#Text/blog/#Text/c/#CommentId/watch

getWatchBlogPostCommentR :: Text -> Text -> CommentId -> Handler Html
getWatchBlogPostCommentR project_handle post_name comment_id = do
    (widget, _) <- makeBlogPostCommentActionWidget makeWatchCommentWidget project_handle post_name comment_id def getMaxDepth

    defaultLayout $ projectBlogDiscussionPage project_handle post_name widget

postWatchBlogPostCommentR ::Text -> Text -> CommentId -> Handler Html
postWatchBlogPostCommentR project_handle post_name comment_id = do
    (viewer@(Entity viewer_id _), _, comment) <- checkCommentRequireAuth project_handle post_name comment_id
    checkBlogPostCommentActionPermission can_watch viewer project_handle (Entity comment_id comment)

    postWatchComment viewer_id comment_id

    redirect $ BlogPostCommentR project_handle post_name comment_id

--------------------------------------------------------------------------------
-- /p/#Text/blog/#Text/c/#CommentId/unwatch

getUnwatchBlogPostCommentR :: Text -> Text -> CommentId -> Handler Html
getUnwatchBlogPostCommentR project_handle post_name comment_id = do
    (widget, _) <- makeBlogPostCommentActionWidget makeUnwatchCommentWidget project_handle post_name comment_id def getMaxDepth

    defaultLayout $ projectBlogDiscussionPage project_handle post_name widget

postUnwatchBlogPostCommentR ::Text -> Text -> CommentId -> Handler Html
postUnwatchBlogPostCommentR project_handle post_name comment_id = do
    (viewer@(Entity viewer_id _), _, comment) <- checkCommentRequireAuth project_handle post_name comment_id
    checkBlogPostCommentActionPermission can_watch viewer project_handle (Entity comment_id comment)

    postUnwatchComment viewer_id comment_id

    redirect $ BlogPostCommentR project_handle post_name comment_id


--------------------------------------------------------------------------------
-- /p/#Text/blog/#Text/d

getBlogPostDiscussionR :: Text -> Text -> Handler Html
getBlogPostDiscussionR project_handle post_name = getDiscussion $ getBlogPostDiscussion project_handle post_name

getBlogPostDiscussion :: Text -> Text -> (DiscussionId -> ExprCommentCond -> DB [Entity Comment]) -> Handler Html
getBlogPostDiscussion project_handle post_name get_root_comments = do
    muser <- maybeAuth
    let muser_id = entityKey <$> muser

    (Entity project_id project, root_comments) <- runYDB $ do
        p@(Entity project_id _) <- getBy404 $ UniqueProjectHandle project_handle
        Entity _ blog_post <- getBy404 $ UniqueBlogPost project_id post_name
        let has_permission = exprCommentProjectPermissionFilter muser_id (val project_id)
        root_comments <- get_root_comments (blogPostDiscussion blog_post) has_permission
        return (p, root_comments)

    (comment_forest_no_css, _) <-
        makeBlogPostCommentForestWidget
            muser
            project_id
            project_handle
            post_name
            root_comments
            def
            getMaxDepth
            False
            mempty

    let has_comments = not (null root_comments)
        comment_forest = do
            comment_forest_no_css
            toWidget $(cassiusFile "templates/comment.cassius")

    (comment_form, _) <- generateFormPost commentNewTopicForm

    defaultLayout $ do
        setTitle . toHtml $ projectName project <> " Discussion | Snowdrift.coop"
        $(widgetFile "project_blog_discuss")

--------------------------------------------------------------------------------
-- /p/#Text/blog/#Text/d/new

getNewBlogPostDiscussionR :: Text -> Text -> Handler Html
getNewBlogPostDiscussionR project_handle post_name = do
    void requireAuth
    let widget = commentNewTopicFormWidget
    defaultLayout $ projectBlogDiscussionPage project_handle post_name widget

postNewBlogPostDiscussionR :: Text -> Text -> Handler Html
postNewBlogPostDiscussionR project_handle post_name = do
    user <- requireAuth
    Entity _ BlogPost{..} <- runYDB $ do
        Entity project_id _ <- getBy404 $ UniqueProjectHandle project_handle
        getBy404 $ UniqueBlogPost project_id post_name

    postNewComment
        Nothing
        user
        blogPostDiscussion
        (makeProjectCommentActionPermissionsMap (Just user) project_handle def) >>= \case
            Left comment_id -> redirect $ BlogPostCommentR project_handle post_name comment_id
            Right (widget, form) -> defaultLayout $ previewWidget form "post" $ projectBlogDiscussionPage project_handle post_name widget

