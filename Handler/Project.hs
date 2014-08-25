{-# LANGUAGE TupleSections, OverloadedStrings #-}

module Handler.Project where

import Import

import Data.Filter
import Data.Order
import Handler.Comment
import Handler.Discussion
import Handler.Utils
import Model.Application
import Model.Comment
import Model.Comment.ActionPermissions
import Model.Comment.HandlerInfo
import Model.Comment.Sql
import Model.Discussion
import Model.Issue
import Model.Markdown
import Model.Markdown.Diff
import Model.Project
import Model.Role
import Model.SnowdriftEvent
import Model.User
import Model.Wiki
import View.Comment
import View.PledgeButton
import View.Project
import View.SnowdriftEvent
import Widgets.Preview
import Widgets.Time

import           Data.Default  (def)
import           Data.List     (sortBy)
import qualified Data.Map      as M
import           Data.Maybe    (maybeToList)
import qualified Data.Set      as S
import qualified Data.Text     as T
import           Data.Tree     (Forest, Tree)
import qualified Data.Tree     as Tree
import           System.Random (randomIO)
import           Text.Cassius  (cassiusFile)
import           Text.Printf

--------------------------------------------------------------------------------
-- Utility functions

lookupGetParamDefault :: Read a => Text -> a -> Handler a
lookupGetParamDefault name def_val = do
    maybe_value <- lookupGetParam name
    return (fromMaybe def_val (maybe_value >>= readMaybe . T.unpack))

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

-- | Sanity check for Project Comment pages. Redirects if the comment was rethreaded.
-- 404's if the comment doesn't exist. 403 if permission denied.
checkComment :: Text -> CommentId -> Handler (Maybe (Entity User), Entity Project, Comment)
checkComment project_handle comment_id = do
    muser <- maybeAuth
    (project, comment) <- checkComment' (entityKey <$> muser) project_handle comment_id
    return (muser, project, comment)

-- | Like checkComment, but authentication is required.
checkCommentRequireAuth :: Text -> CommentId -> Handler (Entity User, Entity Project, Comment)
checkCommentRequireAuth project_handle comment_id = do
    user@(Entity user_id _) <- requireAuth
    (project, comment) <- checkComment' (Just user_id) project_handle comment_id
    return (user, project, comment)

-- | Abstract checkComment and checkCommentRequireAuth. You shouldn't use this function directly.
checkComment' :: Maybe UserId -> Text -> CommentId -> Handler (Entity Project, Comment)
checkComment' muser_id project_handle comment_id = do
    redirectIfRethreaded comment_id

    (project, ecomment) <- runYDB $ do
        project@(Entity project_id _) <- getBy404 (UniqueProjectHandle project_handle)
        let has_permission = exprCommentProjectPermissionFilter muser_id (val project_id)
        ecomment <- fetchCommentDB comment_id has_permission
        return (project, ecomment)

    case ecomment of
        Left CommentNotFound         -> notFound
        Left CommentPermissionDenied -> permissionDenied "You don't have permission to view this comment."
        Right comment                ->
            if commentDiscussion comment /= projectDiscussion (entityVal project)
                then notFound
                else return (project, comment)

checkProjectCommentActionPermission
        :: (CommentActionPermissions -> Bool)
        -> Entity User
        -> Text
        -> Entity Comment
        -> Handler ()
checkProjectCommentActionPermission
        can_perform_action
        user
        project_handle
        comment@(Entity comment_id _) = do
    action_permissions <-
        lookupErr "checkWikiPageCommentActionPermission: comment id not found in map" comment_id
          <$> makeProjectCommentActionPermissionsMap (Just user) project_handle [comment]
    unless (can_perform_action action_permissions)
           (permissionDenied "You don't have permission to perform this action.")

makeProjectCommentForestWidget
        :: Maybe (Entity User)
        -> ProjectId
        -> Text
        -> [Entity Comment]
        -> CommentMods
        -> Handler MaxDepth
        -> Bool
        -> Widget
        -> Handler (Widget, Forest (Entity Comment))
makeProjectCommentForestWidget
        muser
        project_id
        project_handle
        comments
        comment_mods
        get_max_depth
        is_preview
        widget_under_root_comment = do
    makeCommentForestWidget
      (projectCommentHandlerInfo muser project_id project_handle)
      comments
      muser
      comment_mods
      get_max_depth
      is_preview
      widget_under_root_comment

makeProjectCommentTreeWidget
        :: Maybe (Entity User)
        -> ProjectId
        -> Text
        -> Entity Comment
        -> CommentMods
        -> Handler MaxDepth
        -> Bool
        -> Widget
        -> Handler (Widget, Tree (Entity Comment))
makeProjectCommentTreeWidget a b c d e f g h = do
    (widget, [tree]) <- makeProjectCommentForestWidget a b c [d] e f g h
    return (widget, tree)

makeProjectCommentActionWidget
        :: MakeCommentActionWidget
        -> Text
        -> CommentId
        -> CommentMods
        -> Handler MaxDepth
        -> Handler (Widget, Tree (Entity Comment))
makeProjectCommentActionWidget make_comment_action_widget project_handle comment_id mods get_max_depth = do
    (user, Entity project_id _, comment) <- checkCommentRequireAuth project_handle comment_id
    make_comment_action_widget
      (Entity comment_id comment)
      user
      (projectCommentHandlerInfo (Just user) project_id project_handle)
      mods
      get_max_depth
      False

-------------------------------------------------------------------------------
--

getProjectsR :: Handler Html
getProjectsR = do
    projects <- runDB fetchAllProjectsDB
    defaultLayout $ do
        setTitle "Projects | Snowdrift.coop"
        $(widgetFile "projects")

--------------------------------------------------------------------------------
-- /

getProjectR :: Text -> Handler Html
getProjectR project_handle = do
    mviewer_id <- maybeAuthId

    (project_id, project, pledges, pledge) <- runYDB $ do
        Entity project_id project <- getBy404 $ UniqueProjectHandle project_handle
        pledges <- getProjectShares project_id
        pledge <- case mviewer_id of
            Nothing -> return Nothing
            Just viewer_id -> getBy $ UniquePledge viewer_id project_id

        return (project_id, project, pledges, pledge)

    defaultLayout $ do
        setTitle . toHtml $ projectName project <> " | Snowdrift.coop"
        renderProject (Just project_id) project pledges pledge

postProjectR :: Text -> Handler Html
postProjectR project_handle = do
    (viewer_id, Entity project_id project) <-
        requireRolesAny [Admin] project_handle "You do not have permission to edit this project."

    ((result, _), _) <- runFormPost $ editProjectForm Nothing

    now <- liftIO getCurrentTime

    case result of
        FormSuccess (UpdateProject name description tags github_repo) ->
            lookupPostMode >>= \case
                Just PostMode -> do
                    runDB $ do
                        when (projectDescription project /= description) $ do
                            project_update <- insert $ ProjectUpdate now project_id viewer_id $ diffMarkdown (projectDescription project) description
                            last_update <- getBy $ UniqueProjectLastUpdate project_id
                            case last_update of
                                Just (Entity key _) -> repsert key $ ProjectLastUpdate project_id project_update
                                Nothing -> void $ insert $ ProjectLastUpdate project_id project_update

                        update $ \ p -> do
                            set p [ ProjectName =. val name, ProjectDescription =. val description, ProjectGithubRepo =. val github_repo ]
                            where_ (p ^. ProjectId ==. val project_id)

                        tag_ids <- forM tags $ \ tag_name -> do
                            tag_entity_list <- select $ from $ \ tag -> do
                                where_ (tag ^. TagName ==. val tag_name)
                                return tag

                            case tag_entity_list of
                                [] -> insert $ Tag tag_name
                                Entity tag_id _ : _ -> return tag_id


                        delete $
                         from $ \pt ->
                         where_ (pt ^. ProjectTagProject ==. val project_id)

                        forM_ tag_ids $ \tag_id -> insert (ProjectTag project_id tag_id)

                    alertSuccess "project updated"
                    redirect $ ProjectR project_handle

                _ -> do
                    let preview_project = project { projectName = name, projectDescription = description, projectGithubRepo = github_repo }

                    (form, _) <- generateFormPost $ editProjectForm (Just (preview_project, tags))
                    defaultLayout $ previewWidget form "update" $ renderProject (Just project_id) preview_project [] Nothing

        x -> do
            alertDanger (T.pack $ show x)
            redirect (ProjectR project_handle)

--------------------------------------------------------------------------------
-- /application

getApplicationsR :: Text -> Handler Html
getApplicationsR project_handle = do
    viewer_id <- requireAuthId

    (project, applications) <- runYDB $ do
        Entity project_id project <- getBy404 (UniqueProjectHandle project_handle)
        ok <- userIsAffiliatedWithProjectDB viewer_id project_id
        unless ok $
            lift (permissionDenied "You don't have permission to view this page.")

        applications <- fetchProjectVolunteerApplicationsDB project_id
        userReadVolunteerApplicationsDB viewer_id
        return (project, applications)

    defaultLayout $ do
        setTitle . toHtml $ projectName project <> " Volunteer Applications | Snowdrift.coop"
        $(widgetFile "applications")

getApplicationR :: Text -> VolunteerApplicationId -> Handler Html
getApplicationR project_handle application_id = do
    viewer_id <- requireAuthId
    (project, user, application, interests, num_interests) <- runYDB $ do
        Entity project_id project <- getBy404 (UniqueProjectHandle project_handle)
        ok <- userIsAffiliatedWithProjectDB viewer_id project_id
        unless ok $
            lift (permissionDenied "You don't have permission to view this page.")

        application <- get404 application_id
        let user_id = volunteerApplicationUser application
        user <- get404 user_id
        (interests, num_interests) <- (T.intercalate ", " &&& length) <$> fetchApplicationVolunteerInterestsDB application_id
        return (project, Entity user_id user, application, interests, num_interests)

    defaultLayout $ do
        setTitle . toHtml $ projectName project <> " Volunteer Application - " <> userDisplayName user <> " | Snowdrift.coop"
        $(widgetFile "application")

--------------------------------------------------------------------------------
-- /button.png

getProjectPledgeButtonR :: Text -> Handler TypedContent
getProjectPledgeButtonR project_handle = do
   pledges <- runYDB $ do
        Entity project_id _project <- getBy404 $ UniqueProjectHandle project_handle
        getProjectShares project_id
   let png = overlayImage blankPledgeButton $
        fillInPledgeCount (fromIntegral (length pledges))
   respond "image/png" png

--------------------------------------------------------------------------------
-- /b

getProjectBlogR :: Text -> Handler Html
getProjectBlogR project_handle = do
    maybe_from <- fmap (Key . PersistInt64 . read . T.unpack) <$> lookupGetParam "from"
    post_count <- fromMaybe 10 <$> fmap (read . T.unpack) <$> lookupGetParam "from"
    Entity project_id project <- runYDB $ getBy404 $ UniqueProjectHandle project_handle

    let apply_offset blog = maybe id (\ from_blog rest -> blog ^. ProjectBlogId >=. val from_blog &&. rest) maybe_from

    (posts, next) <- fmap (splitAt post_count) $ runDB $
        select $
        from $ \blog -> do
        where_ $ apply_offset blog $ blog ^. ProjectBlogProject ==. val project_id
        orderBy [ desc $ blog ^. ProjectBlogTime, desc $ blog ^. ProjectBlogId ]
        limit (fromIntegral post_count + 1)
        return blog

    renderRouteParams <- getUrlRenderParams

    let nextRoute next_id = renderRouteParams (ProjectBlogR project_handle) [("from", toPathPiece next_id)]

    defaultLayout $ do
        setTitle . toHtml $ projectName project <> " Blog | Snowdrift.coop"

        $(widgetFile "project_blog")


getNewProjectBlogPostR :: Text -> Handler Html
getNewProjectBlogPostR project_handle = do
    (_, Entity _ project) <- requireRolesAny [Admin, TeamMember] project_handle "You do not have permission to post to this project's blog."

    (blog_form, _) <- generateFormPost $ projectBlogForm Nothing

    defaultLayout $ do
        setTitle . toHtml $ "Post To " <> projectName project <> " Blog | Snowdrift.coop"

        $(widgetFile "new_blog_post")


postNewProjectBlogPostR :: Text -> Handler Html
postNewProjectBlogPostR project_handle = do
    (viewer_id, Entity project_id _) <-
        requireRolesAny [Admin, TeamMember] project_handle "You do not have permission to post to this project's blog."

    now <- liftIO getCurrentTime

    ((result, _), _) <- runFormPost $ projectBlogForm Nothing

    case result of
        FormSuccess mk_blog_post -> do
            mode <- lookupPostParam "mode"
            let action :: Text = "post"
            case mode of
                Just "preview" -> do
                    let blog_post :: ProjectBlog
                        blog_post = mk_blog_post now viewer_id project_id (Key $ PersistInt64 0)
                        title = projectBlogTitle blog_post
                        handle = projectBlogHandle blog_post
                        top_content = projectBlogTopContent blog_post
                        bottom_content = fromMaybe "" $ projectBlogBottomContent blog_post
                        content = top_content <> bottom_content

                    (form, _) <- generateFormPost $ projectBlogForm $ Just (title, handle, content)

                    defaultLayout $ previewWidget form action $ renderBlogPost project_handle blog_post

                Just x | x == action -> do
                    void $ runDB $ do
                        discussion_id <- insert $ Discussion 0

                        let blog_post :: ProjectBlog
                            blog_post = mk_blog_post now viewer_id project_id discussion_id

                        insert blog_post

                    alertSuccess "posted"
                    redirect $ ProjectBlogR project_handle

                x -> do
                    addAlertEm "danger" ("unrecognized mode: " <> T.pack (show x)) "Error: "
                    redirect $ NewProjectBlogPostR project_handle

        x -> do
            alertDanger $ T.pack $ show x
            redirect $ NewProjectBlogPostR project_handle


getProjectBlogPostR :: Text -> Text -> Handler Html
getProjectBlogPostR project_handle blog_post_handle = do
    (project, blog_post) <- runYDB $ do
        Entity project_id project <- getBy404 $ UniqueProjectHandle project_handle
        Entity _ blog_post <- getBy404 $ UniqueProjectBlogPost project_id blog_post_handle

        return (project, blog_post)

    defaultLayout $ do
        setTitle . toHtml $ projectName project <> " Blog - " <> projectBlogTitle blog_post <> " | Snowdrift.coop"

        renderBlogPost project_handle blog_post

--------------------------------------------------------------------------------
-- /c/#CommentId

getProjectCommentR :: Text -> CommentId -> Handler Html
getProjectCommentR project_handle comment_id = do
    (muser, Entity project_id _, comment) <- checkComment project_handle comment_id
    (widget, comment_tree) <-
        makeProjectCommentTreeWidget
          muser
          project_id
          project_handle
          (Entity comment_id comment)
          def
          getMaxDepth
          False
          mempty

    case muser of
        Nothing -> return ()
        Just (Entity user_id _) ->
            runDB (userMaybeViewProjectCommentsDB user_id project_id (map entityKey (Tree.flatten comment_tree)))

    defaultLayout $(widgetFile "project_discussion_wrapper")

--------------------------------------------------------------------------------
-- /c/#CommentId/close

getCloseProjectCommentR :: Text -> CommentId -> Handler Html
getCloseProjectCommentR project_handle comment_id = do
    (widget, _) <-
        makeProjectCommentActionWidget
          makeCloseCommentWidget
          project_handle
          comment_id
          def
          getMaxDepth
    defaultLayout $(widgetFile "project_discussion_wrapper")

postCloseProjectCommentR :: Text -> CommentId -> Handler Html
postCloseProjectCommentR project_handle comment_id = do
    (user, (Entity project_id _), comment) <- checkCommentRequireAuth project_handle comment_id
    checkProjectCommentActionPermission can_close user project_handle (Entity comment_id comment)

    postCloseComment
      user
      comment_id
      comment
      (projectCommentHandlerInfo (Just user) project_id project_handle)
      >>= \case
        Nothing -> redirect (ProjectCommentR project_handle comment_id)
        Just (widget, form) -> defaultLayout $ previewWidget form "close" ($(widgetFile "project_discussion_wrapper"))

--------------------------------------------------------------------------------
-- /c/#CommentId/delete

getDeleteProjectCommentR :: Text -> CommentId -> Handler Html
getDeleteProjectCommentR project_handle comment_id = do
    (widget, _) <-
        makeProjectCommentActionWidget
          makeDeleteCommentWidget
          project_handle
          comment_id
          def
          getMaxDepth
    defaultLayout $(widgetFile "project_discussion_wrapper")

postDeleteProjectCommentR :: Text -> CommentId -> Handler Html
postDeleteProjectCommentR project_handle comment_id = do
    (user, _, comment) <- checkCommentRequireAuth project_handle comment_id
    checkProjectCommentActionPermission can_delete user project_handle (Entity comment_id comment)

    was_deleted <- postDeleteComment comment_id
    if was_deleted
        then redirect (ProjectDiscussionR project_handle)
        else redirect (ProjectCommentR project_handle comment_id)

--------------------------------------------------------------------------------
-- /c/#CommentId/edit

getEditProjectCommentR :: Text -> CommentId -> Handler Html
getEditProjectCommentR project_handle comment_id = do
    (widget, _) <-
        makeProjectCommentActionWidget
          makeEditCommentWidget
          project_handle
          comment_id
          def
          getMaxDepth
    defaultLayout $(widgetFile "project_discussion_wrapper")

postEditProjectCommentR :: Text -> CommentId -> Handler Html
postEditProjectCommentR project_handle comment_id = do
    (user, Entity project_id _, comment) <- checkCommentRequireAuth project_handle comment_id
    checkProjectCommentActionPermission can_edit user project_handle (Entity comment_id comment)

    postEditComment
      user
      (Entity comment_id comment)
      (projectCommentHandlerInfo (Just user) project_id project_handle)
      >>= \case
        Nothing -> redirect (ProjectCommentR project_handle comment_id)         -- Edit made.
        Just widget -> defaultLayout $(widgetFile "project_discussion_wrapper") -- Previewing edit.

--------------------------------------------------------------------------------
-- /c/#CommentId/flag

getFlagProjectCommentR :: Text -> CommentId -> Handler Html
getFlagProjectCommentR project_handle comment_id = do
    (widget, _) <-
        makeProjectCommentActionWidget
          makeFlagCommentWidget
          project_handle
          comment_id
          def
          getMaxDepth
    defaultLayout $(widgetFile "project_discussion_wrapper")

postFlagProjectCommentR :: Text -> CommentId -> Handler Html
postFlagProjectCommentR project_handle comment_id = do
    (user, Entity project_id _, comment) <- checkCommentRequireAuth project_handle comment_id
    checkProjectCommentActionPermission can_flag user project_handle (Entity comment_id comment)

    postFlagComment
      user
      (Entity comment_id comment)
      (projectCommentHandlerInfo (Just user) project_id project_handle)
      >>= \case
        Nothing -> redirect (ProjectDiscussionR project_handle)
        Just widget -> defaultLayout $(widgetFile "project_discussion_wrapper")

--------------------------------------------------------------------------------
-- /moderate TODO: rename to /approve

getApproveProjectCommentR :: Text -> CommentId -> Handler Html
getApproveProjectCommentR project_handle comment_id = do
    (widget, _) <-
        makeProjectCommentActionWidget
          makeApproveCommentWidget
          project_handle
          comment_id
          def
          getMaxDepth
    defaultLayout $(widgetFile "project_discussion_wrapper")

postApproveProjectCommentR :: Text -> CommentId -> Handler Html
postApproveProjectCommentR project_handle comment_id = do
    (user@(Entity user_id _), _, comment) <- checkCommentRequireAuth project_handle comment_id
    checkProjectCommentActionPermission can_approve user project_handle (Entity comment_id comment)

    postApproveComment user_id comment_id comment
    redirect (ProjectCommentR project_handle comment_id)

--------------------------------------------------------------------------------
-- /c/#CommentId/reply

getReplyProjectCommentR :: Text -> CommentId -> Handler Html
getReplyProjectCommentR project_handle parent_id = do
    (widget, _) <-
        makeProjectCommentActionWidget
          makeReplyCommentWidget
          project_handle
          parent_id
          def
          getMaxDepth
    defaultLayout $(widgetFile "project_discussion_wrapper")

postReplyProjectCommentR :: Text -> CommentId -> Handler Html
postReplyProjectCommentR project_handle parent_id = do
    (user, Entity _ project, parent) <- checkCommentRequireAuth project_handle parent_id
    checkProjectCommentActionPermission can_reply user project_handle (Entity parent_id parent)

    postNewComment
      (Just parent_id)
      user
      (projectDiscussion project)
      (makeProjectCommentActionPermissionsMap (Just user) project_handle) >>= \case
        Left _ -> redirect (ProjectCommentR project_handle parent_id)
        Right (widget, form) -> defaultLayout $ previewWidget form "post" ($(widgetFile "project_discussion_wrapper"))

--------------------------------------------------------------------------------
-- /c/#CommentId/rethread

getRethreadProjectCommentR :: Text -> CommentId -> Handler Html
getRethreadProjectCommentR project_handle comment_id = do
    (widget, _) <-
        makeProjectCommentActionWidget
          makeRethreadCommentWidget
          project_handle
          comment_id
          def
          getMaxDepth
    defaultLayout $(widgetFile "project_discussion_wrapper")

postRethreadProjectCommentR :: Text -> CommentId -> Handler Html
postRethreadProjectCommentR project_handle comment_id = do
    (user@(Entity user_id _), _, comment) <- checkCommentRequireAuth project_handle comment_id
    checkProjectCommentActionPermission can_rethread user project_handle (Entity comment_id comment)
    postRethreadComment user_id comment_id comment

--------------------------------------------------------------------------------
-- /c/#CommentId/retract

getRetractProjectCommentR :: Text -> CommentId -> Handler Html
getRetractProjectCommentR project_handle comment_id = do
    (widget, _) <-
        makeProjectCommentActionWidget
          makeRetractCommentWidget
          project_handle
          comment_id
          def
          getMaxDepth
    defaultLayout $(widgetFile "project_discussion_wrapper")

postRetractProjectCommentR :: Text -> CommentId -> Handler Html
postRetractProjectCommentR project_handle comment_id = do
    (user, Entity project_id _, comment) <- checkCommentRequireAuth project_handle comment_id
    checkProjectCommentActionPermission can_retract user project_handle (Entity comment_id comment)

    postRetractComment
      user
      comment_id
      comment
      (projectCommentHandlerInfo (Just user) project_id project_handle)
      >>= \case
        Nothing -> redirect (ProjectCommentR project_handle comment_id)
        Just (widget, form) -> defaultLayout $ previewWidget form "retract" ($(widgetFile "project_discussion_wrapper"))

--------------------------------------------------------------------------------
-- /c/#CommentId/tags

getProjectCommentTagsR :: Text -> CommentId -> Handler Html
getProjectCommentTagsR _ = getCommentTags

--------------------------------------------------------------------------------
-- /c/#CommentId/tag/#TagId

getProjectCommentTagR :: Text -> CommentId -> TagId -> Handler Html
getProjectCommentTagR _ = getCommentTagR

postProjectCommentTagR :: Text -> CommentId -> TagId -> Handler Html
postProjectCommentTagR project_handle comment_id tag_id = do
    postCommentTag comment_id tag_id
    redirect (ProjectCommentTagR project_handle comment_id tag_id)

--------------------------------------------------------------------------------
-- /c/#CommentId/tag/apply, /c/#CommentId/tag/create

postProjectCommentApplyTagR, postProjectCommentCreateTagR:: Text -> CommentId -> Handler Html
postProjectCommentApplyTagR  = applyOrCreate postCommentApplyTag
postProjectCommentCreateTagR = applyOrCreate postCommentCreateTag

applyOrCreate :: (CommentId -> Handler ()) -> Text -> CommentId -> Handler Html
applyOrCreate action project_handle comment_id = do
    action comment_id
    redirect (ProjectCommentR project_handle comment_id)

--------------------------------------------------------------------------------
-- /c/#CommentId/tag/new

getProjectCommentAddTagR :: Text -> CommentId -> Handler Html
getProjectCommentAddTagR project_handle comment_id = do
    (user@(Entity user_id _), Entity project_id _, comment) <- checkCommentRequireAuth project_handle comment_id
    checkProjectCommentActionPermission can_add_tag user project_handle (Entity comment_id comment)
    getProjectCommentAddTag comment_id project_id user_id

--------------------------------------------------------------------------------
-- /d

getProjectDiscussionR :: Text -> Handler Html
getProjectDiscussionR = getDiscussion . getProjectDiscussion

getProjectDiscussion :: Text -> (DiscussionId -> ExprCommentCond -> DB [Entity Comment]) -> Handler Html
getProjectDiscussion project_handle get_root_comments = do
    muser <- maybeAuth
    let muser_id = entityKey <$> muser

    (Entity project_id project, root_comments) <- runYDB $ do
        p@(Entity project_id project) <- getBy404 (UniqueProjectHandle project_handle)
        let has_permission = exprCommentProjectPermissionFilter muser_id (val project_id)
        root_comments <- get_root_comments (projectDiscussion project) has_permission
        return (p, root_comments)

    (comment_forest_no_css, _) <-
        makeProjectCommentForestWidget
          muser
          project_id
          project_handle
          root_comments
          def
          (getMaxDepthDefault 0)
          False
          mempty

    let has_comments = not (null root_comments)
        comment_forest = do
            comment_forest_no_css
            toWidget $(cassiusFile "templates/comment.cassius")

    (comment_form, _) <- generateFormPost commentNewTopicForm

    defaultLayout $ do
        setTitle . toHtml $ projectName project <> " Discussion | Snowdrift.coop"
        $(widgetFile "project_discuss")

--------------------------------------------------------------------------------
-- /d/new

getNewProjectDiscussionR :: Text -> Handler Html
getNewProjectDiscussionR project_handle = do
    void requireAuth
    let widget = commentNewTopicFormWidget
    defaultLayout $(widgetFile "project_discussion_wrapper")

postNewProjectDiscussionR :: Text -> Handler Html
postNewProjectDiscussionR project_handle = do
    user <- requireAuth
    Entity _ Project{..} <- runYDB (getBy404 (UniqueProjectHandle project_handle))

    postNewComment
      Nothing
      user
      projectDiscussion
      (makeProjectCommentActionPermissionsMap (Just user) project_handle) >>= \case
        Left comment_id -> redirect (ProjectCommentR project_handle comment_id)
        Right (widget, form) -> defaultLayout $ previewWidget form "post" ($(widgetFile "project_discussion_wrapper"))

--------------------------------------------------------------------------------
-- /edit

getEditProjectR :: Text -> Handler Html
getEditProjectR project_handle = do
    (_, Entity project_id project) <-
        requireRolesAny [Admin] project_handle "You do not have permission to edit this project."

    tags <- runDB $
        select $
        from $ \ (p_t `InnerJoin` tag) -> do
        on_ (p_t ^. ProjectTagTag ==. tag ^. TagId)
        where_ (p_t ^. ProjectTagProject ==. val project_id)
        return tag

    (project_form, _) <- generateFormPost $ editProjectForm (Just (project, map (tagName . entityVal) tags))

    defaultLayout $ do
        setTitle . toHtml $ projectName project <> " | Snowdrift.coop"
        $(widgetFile "edit_project")

--------------------------------------------------------------------------------
-- /feed

-- | This function is responsible for hitting every relevant event table. Nothing
-- statically guarantees that.
getProjectFeedR :: Text -> Handler Html
getProjectFeedR project_handle = do
    let lim = 26 -- limit n from each table, then take (n-1)

    muser <- maybeAuth
    let muser_id = entityKey <$> muser


    before <- lookupGetUTCTimeDefaultNow "before"

    (project, comment_entities, wiki_pages, wiki_edit_entities,
     new_pledges, updated_pledges, deleted_pledges,
     discussion_wiki_page_map, wiki_page_map, user_map,
     earlier_closures_map, closure_map, ticket_map, flag_map) <- runYDB $ do

        Entity project_id project <- getBy404 (UniqueProjectHandle project_handle)

        project_comments   <- fetchProjectCommentsBeforeDB         project_id muser_id before lim
        wiki_page_comments <- fetchProjectWikiPageCommentsBeforeDB project_id muser_id before lim
        wiki_pages         <- fetchProjectWikiPagesBeforeDB        project_id before lim
        wiki_edit_entities <- fetchProjectWikiEditsBeforeDB        project_id before lim
        new_pledges        <- fetchProjectNewPledgesBeforeDB       project_id before lim
        updated_pledges    <- fetchProjectUpdatedPledgesBeforeDB   project_id before lim
        deleted_pledges    <- fetchProjectDeletedPledgesBeforeDB   project_id before lim

        -- Suplementary maps for displaying the data. If something above requires extra
        -- data to display the project feed row, it MUST be used to fetch the data below!
        -- The Maybes from Data.Map.lookup are unsafely STRIPPED in the views!

        let comment_entities = project_comments <> wiki_page_comments
            comment_ids      = map entityKey comment_entities
            comments         = map entityVal comment_entities
            wiki_edits       = map entityVal wiki_edit_entities
            shares_pledged   = map entityVal (new_pledges <> (map snd updated_pledges))
            -- All users: Comment posters, WikiPage creators, WikiEdit makers,
            -- and Pledgers (new, updated, and deleted).
            user_ids = S.toList $
                         S.fromList (map commentUser comments) <>
                         S.fromList (map wikiEditUser wiki_edits) <>
                         S.fromList (map sharesPledgedUser shares_pledged) <>
                         S.fromList (map eventDeletedPledgeUser deleted_pledges)

        -- WikiPages that can be keyed by a Comment's DiscussionId (i.e. the Comment *is* on a WikiPage)
        discussion_wiki_page_map <- M.fromList . map (\e@(Entity _ WikiPage{..}) -> (wikiPageDiscussion, e)) <$>
                                       fetchDiscussionWikiPagesInDB (map commentDiscussion comments)

        -- WikiPages keyed by their own IDs (contained in a WikiEdit)
        wiki_page_map <- entitiesMap <$> fetchWikiPagesInDB (map wikiEditPage wiki_edits)

        user_map <- entitiesMap <$> fetchUsersInDB user_ids

        earlier_closures_map <- fetchCommentsAncestorClosuresDB comment_ids
        closure_map          <- makeClosureMapDB comment_ids
        ticket_map           <- makeTicketMapDB  comment_ids
        flag_map             <- makeFlagMapDB    comment_ids


        return (project, comment_entities, wiki_pages, wiki_edit_entities,
                new_pledges, updated_pledges, deleted_pledges,
                discussion_wiki_page_map, wiki_page_map, user_map,
                earlier_closures_map, closure_map, ticket_map, flag_map)

    action_permissions_map <- makeProjectCommentActionPermissionsMap muser project_handle comment_entities

    let all_unsorted_events = mconcat
            [ map (onEntity ECommentPosted)  comment_entities
            , map (onEntity EWikiPage)       wiki_pages
            , map (onEntity EWikiEdit)       wiki_edit_entities
            , map (onEntity ENewPledge)      new_pledges
            , map eup2se                     updated_pledges
            , map edp2se                     deleted_pledges
            ]

        (events, more_events) = splitAt (lim-1) (sortBy snowdriftEventNewestToOldest all_unsorted_events)

        -- For pagination: Nothing means no more pages, Just time means set the 'before'
        -- GET param to that time. Note that this means 'before' should be a <= relation,
        -- rather than a <.
        mnext_before :: Maybe Text
        mnext_before = case more_events of
          []             -> Nothing
          (next_event:_) -> (Just . T.pack . show . snowdriftEventTime) next_event

    defaultLayout $ do
        $(widgetFile "project_feed")
        toWidget $(cassiusFile "templates/comment.cassius")
  where
    -- "event updated pledge to snowdrift event". Makes above code cleaner.
    eup2se :: (Int64, Entity SharesPledged) -> SnowdriftEvent
    eup2se (old_shares, Entity shares_pledged_id shares_pledged) = EUpdatedPledge old_shares shares_pledged_id shares_pledged

    -- "event deleted pledge to snowdrift event". Makes above code cleaner.
    edp2se :: EventDeletedPledge -> SnowdriftEvent
    edp2se (EventDeletedPledge a b c d) = EDeletedPledge a b c d

--------------------------------------------------------------------------------
-- /invite

getInviteR :: Text -> Handler Html
getInviteR project_handle = do
    (_, Entity _ project) <- requireRolesAny [Admin] project_handle "You must be a project admin to invite."

    now <- liftIO getCurrentTime
    maybe_invite_code <- lookupSession "InviteCode"
    maybe_invite_role <- fmap (read . T.unpack) <$> lookupSession "InviteRole"
    deleteSession "InviteCode"
    deleteSession "InviteRole"
    let maybe_link = InvitationR project_handle <$> maybe_invite_code
    (invite_form, _) <- generateFormPost inviteForm

    outstanding_invites <- runDB $
        select $
        from $ \ invite -> do
        where_ ( invite ^. InviteRedeemed ==. val False )
        orderBy [ desc (invite ^. InviteCreatedTs) ]
        return invite

    redeemed_invites <- runDB $
        select $
        from $ \ invite -> do
        where_ ( invite ^. InviteRedeemed ==. val True )
        orderBy [ desc (invite ^. InviteCreatedTs) ]
        limit 20
        return invite

    let redeemed_users = S.fromList $ mapMaybe (inviteRedeemedBy . entityVal) redeemed_invites
        redeemed_inviters = S.fromList $ map (inviteUser . entityVal) redeemed_invites
        outstanding_inviters = S.fromList $ map (inviteUser . entityVal) outstanding_invites
        user_ids = S.toList $ redeemed_users `S.union` redeemed_inviters `S.union` outstanding_inviters

    user_entities <- runDB $ selectList [ UserId <-. user_ids ] []

    let users = M.fromList $ map (entityKey &&& id) user_entities

    let format_user Nothing = "NULL"
        format_user (Just user_id) =
            let Entity _ user = fromMaybe (error "getInviteR: user_id not found in users map")
                                          (M.lookup user_id users)
             in fromMaybe (userIdent user) $ userName user

        format_inviter user_id =
            userDisplayName $ fromMaybe (error "getInviteR(#2): user_id not found in users map")
                                        (M.lookup user_id users)

    defaultLayout $ do
        setTitle . toHtml $ projectName project <> " - Send Invite | Snowdrift.coop"
        $(widgetFile "invite")

postInviteR :: Text -> Handler Html
postInviteR project_handle = do
    (user_id, Entity project_id _) <- requireRolesAny [Admin] project_handle "You must be a project admin to invite."

    now <- liftIO getCurrentTime
    invite <- liftIO randomIO

    ((result, _), _) <- runFormPost inviteForm
    case result of
        FormSuccess (tag, role) -> do
            let invite_code = T.pack $ printf "%016x" (invite :: Int64)
            _ <- runDB $ insert $ Invite now project_id invite_code user_id role tag False Nothing Nothing
            setSession "InviteCode" invite_code
            setSession "InviteRole" (T.pack $ show role)

        _ -> alertDanger "Error in submitting form."

    redirect $ InviteR project_handle

--------------------------------------------------------------------------------
-- /patrons

getProjectPatronsR :: Text -> Handler Html
getProjectPatronsR project_handle = do
    _ <- requireAuthId

    page <- lookupGetParamDefault "page" 0
    per_page <- lookupGetParamDefault "count" 20

    (project, pledges, user_payouts_map) <- runYDB $ do
        Entity project_id project <- getBy404 $ UniqueProjectHandle project_handle
        pledges <- select $ from $ \ (pledge `InnerJoin` user) -> do
            on_ $ pledge ^. PledgeUser ==. user ^. UserId
            where_ $ pledge ^. PledgeProject ==. val project_id
                &&. pledge ^. PledgeFundedShares >. val 0
            orderBy [ desc (pledge ^. PledgeFundedShares), asc (user ^. UserName), asc (user ^. UserId)]
            offset page
            limit per_page
            return (pledge, user)

        last_paydays <- case projectLastPayday project of
            Nothing -> return []
            Just last_payday -> select $ from $ \ payday -> do
                where_ $ payday ^. PaydayId <=. val last_payday
                orderBy [ desc $ payday ^. PaydayId ]
                limit 2
                return payday

        user_payouts <- select $ from $ \ (transaction `InnerJoin` user) -> do
            where_ $ transaction ^. TransactionPayday `in_` valList (map (Just . entityKey) last_paydays)
            on_ $ transaction ^. TransactionDebit ==. just (user ^. UserAccount)
            groupBy $ user ^. UserId
            return (user ^. UserId, count $ transaction ^. TransactionId)

        return (project, pledges, M.fromList $ map ((\ (Value x :: Value UserId) -> x) *** (\ (Value x :: Value Int) -> x)) user_payouts)

    defaultLayout $ do
        setTitle . toHtml $ projectName project <> " Patrons | Snowdrift.coop"
        $(widgetFile "project_patrons")

--------------------------------------------------------------------------------
-- /t

getTicketsR :: Text -> Handler Html
getTicketsR project_handle = do
    muser_id <- maybeAuthId
    (project, tagged_tickets) <- runYDB $ do
        Entity project_id project <- getBy404 (UniqueProjectHandle project_handle)
        tagged_tickets <- fetchProjectTaggedTicketsDB project_id muser_id
        return (project, tagged_tickets)

    ((result, formWidget), encType) <- runFormGet viewForm
    let (filter_expression, order_expression) = case result of
            FormSuccess x -> x
            _ -> (defaultFilter, defaultOrder)

    github_issues <- getGithubIssues project

    let issues = sortBy (flip compare `on` order_expression . issueOrderable) $
                   filter (filter_expression . issueFilterable) $
                      map mkSomeIssue tagged_tickets ++ map mkSomeIssue github_issues

    defaultLayout $ do
        setTitle . toHtml $ projectName project <> " Tickets | Snowdrift.coop"
        $(widgetFile "tickets")

--------------------------------------------------------------------------------
-- /transactions

getProjectTransactionsR :: Text -> Handler Html
getProjectTransactionsR project_handle = do
    (project, account, account_map, transaction_groups) <- runYDB $ do
        Entity _ project :: Entity Project <- getBy404 $ UniqueProjectHandle project_handle

        account <- get404 $ projectAccount project

        transactions <- select $ from $ \ t -> do
            where_ $ t ^. TransactionCredit ==. val (Just $ projectAccount project)
                    ||. t ^. TransactionDebit ==. val (Just $ projectAccount project)

            orderBy [ desc $ t ^. TransactionTs ]
            return t

        let accounts = S.toList $ S.fromList $ concatMap (\ (Entity _ t) -> maybeToList (transactionCredit t) <> maybeToList (transactionDebit t)) transactions

        users_by_account <- fmap (M.fromList . map (userAccount . entityVal &&& Right)) $ select $ from $ \ u -> do
            where_ $ u ^. UserAccount `in_` valList accounts
            return u

        projects_by_account <- fmap (M.fromList . map (projectAccount . entityVal &&& Left)) $ select $ from $ \ p -> do
            where_ $ p ^. ProjectAccount `in_` valList accounts
            return p

        let account_map = projects_by_account `M.union` users_by_account

        payday_map <- fmap (M.fromList . map (entityKey &&& id)) $ select $ from $ \ pd -> do
            where_ $ pd ^. PaydayId `in_` valList (S.toList $ S.fromList $ mapMaybe (transactionPayday . entityVal) transactions)
            return pd

        return (project, account, account_map, process payday_map transactions)

    let getOtherAccount transaction
            | transactionCredit transaction == Just (projectAccount project) = transactionDebit transaction
            | transactionDebit transaction == Just (projectAccount project) = transactionCredit transaction
            | otherwise = Nothing

    defaultLayout $ do
        setTitle . toHtml $ projectName project <> " Transactions | Snowdrift.coop"
        $(widgetFile "project_transactions")

  where
    process payday_map =
        let process' [] [] = []
            process' (t':ts') [] = [(fmap (payday_map M.!) $ transactionPayday $ entityVal t', reverse (t':ts'))]
            process' [] (t:ts) = process' [t] ts

            process' (t':ts') (t:ts)
                | transactionPayday (entityVal t') == transactionPayday (entityVal t)
                = process' (t:t':ts') ts
                | otherwise
                = (fmap (payday_map M.!) $ transactionPayday $ entityVal t', reverse (t':ts')) : process' [t] ts
         in process' []

--------------------------------------------------------------------------------
-- /w

getWikiPagesR :: Text -> Handler Html
getWikiPagesR project_handle = do
    muser_id <- maybeAuthId
    -- TODO: should be be using unviewed_comments and unviewed_edits?
    (project, pages, _, _) <- runYDB $ do
        Entity project_id project <- getBy404 $ UniqueProjectHandle project_handle
        pages <- getProjectWikiPages project_id

        -- If the user is not logged in or not watching the project, these maps are empty.
        (unviewed_comments, unviewed_edits) <- case muser_id of
            Nothing -> return (mempty, mempty)
            Just user_id -> do
                is_watching <- userIsWatchingProjectDB user_id project_id
                if is_watching
                    then (,)
                        <$> fetchNumUnviewedCommentsOnProjectWikiPagesDB user_id project_id
                        <*> fetchNumUnviewedWikiEditsOnProjectDB         user_id project_id
                    else return (mempty, mempty)
        return (project, pages, unviewed_comments, unviewed_edits)
    defaultLayout $ do
        setTitle . toHtml $ projectName project <> " Wiki | Snowdrift.coop"
        $(widgetFile "wiki_pages")

--------------------------------------------------------------------------------
-- /watch, /unwatch

postWatchProjectR, postUnwatchProjectR :: ProjectId -> Handler ()
postWatchProjectR   = watchOrUnwatchProject userWatchProjectDB   "watching "
postUnwatchProjectR = watchOrUnwatchProject userUnwatchProjectDB "no longer watching "

watchOrUnwatchProject :: (UserId -> ProjectId -> DB ()) -> Text -> ProjectId -> Handler ()
watchOrUnwatchProject action msg project_id = do
    user_id <- requireAuthId
    project <- runYDB $ do
        action user_id project_id
        get404 project_id
    alertSuccess (msg <> projectName project)
    redirect HomeR
