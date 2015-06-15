-- | Handler for comments on Wiki pages. Section comments are relative to /p/#handle/w/#target/c/#comment

module Handler.Wiki.Comment where

import Import

import           Handler.Comment
import           Handler.Project (checkProjectCommentActionPermission)
import           Model.Comment
import           Model.Comment.ActionPermissions
import           Model.Comment.HandlerInfo
import           Model.Comment.Mods
import           Model.Comment.Sql
import           Model.User
import           Widgets.Preview

import           Data.Default                    (def)
import           Data.Tree                       (Forest, Tree)
import qualified Data.Tree                       as Tree
import           Text.Cassius (cassiusFile)

--------------------------------------------------------------------------------
-- Utility functions

-- | Convenience method for all pages that accept a project handle, target, and comment id
-- as URL parameters. Makes sure that the comment is indeed on the page. Redirects if the
-- comment was rethreaded. 404's if the comment doesn't exist. 403 if permission denied.
checkCommentPage :: Text -> Language -> Text -> CommentId -> Handler (Maybe (Entity User), Entity Project, Entity WikiPage, Comment)
checkCommentPage project_handle language target comment_id = do
    muser <- maybeAuth
    (project, page, comment) <- checkCommentPage' (entityKey <$> muser) project_handle language target comment_id
    return (muser, project, page, comment)

-- | Like checkCommentPage, but authentication is required.
checkCommentPageRequireAuth :: Text -> Language -> Text -> CommentId -> Handler (Entity User, Entity Project, Entity WikiPage, Comment)
checkCommentPageRequireAuth project_handle language target comment_id = do
    user@(Entity user_id _) <- requireAuth
    (project, page, comment) <- checkCommentPage' (Just user_id) project_handle language target comment_id
    return (user, project, page, comment)

-- | Abstract checkCommentPage and checkCommentPageRequireAuth. You shouldn't
-- use this function directly.
checkCommentPage' :: Maybe UserId -> Text -> Language -> Text -> CommentId -> Handler (Entity Project, Entity WikiPage, Comment)
checkCommentPage' muser_id project_handle language target comment_id = do
    redirectIfRethreaded comment_id

    (project, wiki_page, ecomment) <- runYDB $ do
        project@(Entity project_id _) <- getBy404 $ UniqueProjectHandle project_handle
        Entity _ wiki_target <- getBy404 $ UniqueWikiTarget (entityKey project) language target
        let wiki_page_id = wikiTargetPage wiki_target
        wiki_page <- get404 wiki_page_id
        let has_permission = exprCommentProjectPermissionFilter muser_id (val project_id)
        ecomment <- fetchCommentDB comment_id has_permission
        return (project, Entity wiki_page_id wiki_page, ecomment)

    case ecomment of
        Left CommentNotFound         -> notFound
        Left CommentPermissionDenied -> permissionDenied "You don't have permission to view this comment."
        Right comment                ->
            if commentDiscussion comment /= wikiPageDiscussion (entityVal wiki_page)
                then notFound
                else return (project, wiki_page, comment)

makeWikiPageCommentForestWidget
        :: Maybe (Entity User)
        -> ProjectId
        -> Text
        -> Language
        -> Text
        -> [Entity Comment]
        -> CommentMods
        -> Handler MaxDepth
        -> Bool
        -> Widget
        -> Handler (Widget, Forest (Entity Comment))
makeWikiPageCommentForestWidget
        muser
        project_id
        project_handle
        language
        target
        comments =
    makeCommentForestWidget
      (wikiPageCommentHandlerInfo muser project_id project_handle language target)
      comments
      muser

makeWikiPageCommentTreeWidget
        :: Maybe (Entity User)
        -> ProjectId
        -> Text
        -> Language
        -> Text
        -> Entity Comment
        -> CommentMods
        -> Handler MaxDepth
        -> Bool
        -> Widget
        -> Handler (Widget, Tree (Entity Comment))
makeWikiPageCommentTreeWidget mviewer project_id project_handle language target comment mods maxDepth is_preview widget_under_root_comment = do
    (widget, [tree]) <- makeWikiPageCommentForestWidget mviewer project_id project_handle language target [comment] mods maxDepth is_preview widget_under_root_comment
    return (widget, tree)

makeWikiPageCommentActionWidget
        :: MakeCommentActionWidget
        -> Text
        -> Language
        -> Text
        -> CommentId
        -> CommentMods
        -> Handler MaxDepth
        -> Handler (Widget, Tree (Entity Comment))
makeWikiPageCommentActionWidget make_comment_action_widget project_handle language target comment_id mods get_max_depth = do
    (user, Entity project_id _, _, comment) <- checkCommentPageRequireAuth project_handle language target comment_id
    make_comment_action_widget
      (Entity comment_id comment)
      user
      (wikiPageCommentHandlerInfo (Just user) project_id project_handle language target)
      mods
      get_max_depth
      False

wikiDiscussionPage :: Text -> Language -> Text -> Widget -> Widget
wikiDiscussionPage project_handle language target widget = do
    $(widgetFile "wiki_discussion_wrapper")
    toWidget $(cassiusFile "templates/comment.cassius")

--------------------------------------------------------------------------------
-- /

getWikiCommentR :: Text -> Language -> Text -> CommentId -> Handler Html
getWikiCommentR project_handle language target comment_id = do
    (muser, Entity project_id _, _, comment) <- checkCommentPage project_handle language target comment_id
    (widget, comment_tree) <-
        makeWikiPageCommentTreeWidget
            muser
            project_id
            project_handle
            language
            target
            (Entity comment_id comment)
            def
            getMaxDepth
            False
            mempty

    case muser of
        Nothing -> return ()
        Just (Entity user_id _) ->
            runDB (userMaybeViewProjectCommentsDB user_id project_id (map entityKey (Tree.flatten comment_tree)))

    defaultLayout (wikiDiscussionPage project_handle language target widget)

--------------------------------------------------------------------------------
-- /claim

getClaimWikiCommentR :: Text -> Language -> Text -> CommentId -> Handler Html
getClaimWikiCommentR project_handle language target comment_id = do
    (widget, _) <-
        makeWikiPageCommentActionWidget
            makeClaimCommentWidget
            project_handle
            language
            target
            comment_id
            def
            getMaxDepth

    defaultLayout (wikiDiscussionPage project_handle language target widget)

postClaimWikiCommentR :: Text -> Language -> Text -> CommentId -> Handler Html
postClaimWikiCommentR project_handle language target comment_id = do
    (user, Entity project_id _, _, comment) <- checkCommentPageRequireAuth project_handle language target comment_id
    checkProjectCommentActionPermission can_claim user project_handle (Entity comment_id comment)

    postClaimComment
      user
      comment_id
      comment
      (wikiPageCommentHandlerInfo (Just user) project_id project_handle language target)
      >>= \case
        Nothing -> redirect (WikiCommentR project_handle language target comment_id)
        Just (widget, form) -> defaultLayout $ previewWidget form "claim" (wikiDiscussionPage project_handle language target widget)

--------------------------------------------------------------------------------
-- /approve

getApproveWikiCommentR :: Text -> Language -> Text -> CommentId -> Handler Html
getApproveWikiCommentR project_handle language target comment_id = do
    (widget, _) <-
        makeWikiPageCommentActionWidget
            makeApproveCommentWidget
            project_handle
            language
            target
            comment_id
            def
            getMaxDepth

    defaultLayout (wikiDiscussionPage project_handle language target widget)

postApproveWikiCommentR :: Text -> Language -> Text -> CommentId -> Handler Html
postApproveWikiCommentR project_handle language target comment_id = do
    (user@(Entity user_id _), _, _, comment) <- checkCommentPageRequireAuth project_handle language target comment_id
    checkProjectCommentActionPermission can_approve user project_handle (Entity comment_id comment)

    postApproveComment user_id comment_id comment
    redirect (WikiCommentR project_handle language target comment_id)

--------------------------------------------------------------------------------
-- /close

getCloseWikiCommentR :: Text -> Language -> Text -> CommentId -> Handler Html
getCloseWikiCommentR project_handle language target comment_id = do
    (widget, _) <-
        makeWikiPageCommentActionWidget
            makeCloseCommentWidget
            project_handle
            language
            target
            comment_id
            def
            getMaxDepth

    defaultLayout (wikiDiscussionPage project_handle language target widget)

postCloseWikiCommentR :: Text -> Language -> Text -> CommentId -> Handler Html
postCloseWikiCommentR project_handle language target comment_id = do
    (user, Entity project_id _, _, comment) <- checkCommentPageRequireAuth project_handle language target comment_id
    checkProjectCommentActionPermission can_close user project_handle (Entity comment_id comment)

    postCloseComment
      user
      comment_id
      comment
      (wikiPageCommentHandlerInfo (Just user) project_id project_handle language target)
      >>= \case
        Nothing -> redirect (WikiCommentR project_handle language target comment_id)
        Just (widget, form) -> defaultLayout $ previewWidget form "close" (wikiDiscussionPage project_handle language target widget)

--------------------------------------------------------------------------------
-- /delete

getDeleteWikiCommentR :: Text -> Language -> Text -> CommentId -> Handler Html
getDeleteWikiCommentR project_handle language target comment_id = do
    (widget, _) <-
        makeWikiPageCommentActionWidget
            makeDeleteCommentWidget
            project_handle
            language
            target
            comment_id
            def
            getMaxDepth

    defaultLayout (wikiDiscussionPage project_handle language target widget)

postDeleteWikiCommentR :: Text -> Language -> Text -> CommentId -> Handler Html
postDeleteWikiCommentR project_handle language target comment_id = do
    (user, _, _, comment) <- checkCommentPageRequireAuth project_handle language target comment_id
    checkProjectCommentActionPermission can_delete user project_handle (Entity comment_id comment)

    was_deleted <- postDeleteComment comment_id
    if was_deleted
        then redirect (WikiDiscussionR project_handle language target)
        else redirect (WikiCommentR project_handle language target comment_id)

--------------------------------------------------------------------------------
-- /edit

getEditWikiCommentR :: Text -> Language -> Text -> CommentId -> Handler Html
getEditWikiCommentR project_handle language target comment_id = do
    (widget, _) <-
        makeWikiPageCommentActionWidget
            makeEditCommentWidget
            project_handle
            language
            target
            comment_id
            def
            getMaxDepth

    defaultLayout (wikiDiscussionPage project_handle language target widget)


postEditWikiCommentR :: Text -> Language -> Text -> CommentId -> Handler Html
postEditWikiCommentR project_handle language target comment_id = do
    (user, Entity project_id _, _, comment) <- checkCommentPageRequireAuth project_handle language target comment_id
    checkProjectCommentActionPermission can_edit user project_handle (Entity comment_id comment)

    postEditComment
      user
      (Entity comment_id comment)
      (wikiPageCommentHandlerInfo (Just user) project_id project_handle language target)
      >>= \case
        Nothing -> redirect (WikiCommentR project_handle language target comment_id)  -- Edit made.
        Just (widget, form) -> defaultLayout $ previewWidget form "post" (wikiDiscussionPage project_handle language target widget)

--------------------------------------------------------------------------------
-- /flag

getFlagWikiCommentR :: Text -> Language -> Text -> CommentId -> Handler Html
getFlagWikiCommentR project_handle language target comment_id = do
    (widget, _) <-
        makeWikiPageCommentActionWidget
            makeFlagCommentWidget
            project_handle
            language
            target
            comment_id
            def
            getMaxDepth

    defaultLayout (wikiDiscussionPage project_handle language target widget)

postFlagWikiCommentR :: Text -> Language -> Text -> CommentId -> Handler Html
postFlagWikiCommentR project_handle language target comment_id = do
    (user, Entity project_id _, _, comment) <- checkCommentPageRequireAuth project_handle language target comment_id
    checkProjectCommentActionPermission can_flag user project_handle (Entity comment_id comment)

    postFlagComment
      user
      (Entity comment_id comment)
      (wikiPageCommentHandlerInfo (Just user) project_id project_handle language target)
      >>= \case
        Nothing -> redirect (WikiDiscussionR project_handle language target)
        Just (widget, form) -> defaultLayout $ previewWidget form "flag" (wikiDiscussionPage project_handle language target widget)

--------------------------------------------------------------------------------
-- /reply

getReplyWikiCommentR :: Text -> Language -> Text -> CommentId -> Handler Html
getReplyWikiCommentR project_handle language target comment_id = do
    (widget, _) <-
        makeWikiPageCommentActionWidget
            makeReplyCommentWidget
            project_handle
            language
            target
            comment_id
            def
            getMaxDepth

    defaultLayout (wikiDiscussionPage project_handle language target widget)

postReplyWikiCommentR :: Text -> Language -> Text -> CommentId -> Handler Html
postReplyWikiCommentR project_handle language target parent_id = do
    (user, _, Entity _ page, parent) <- checkCommentPageRequireAuth project_handle language target parent_id
    checkProjectCommentActionPermission can_reply user project_handle (Entity parent_id parent)

    postNewComment
      (Just parent_id)
      user
      (wikiPageDiscussion page)
      (makeProjectCommentActionPermissionsMap (Just user) project_handle def) >>= \case
        Left _ -> redirect (WikiCommentR project_handle language target parent_id)
        Right (widget, form) -> defaultLayout $ previewWidget form "post" (wikiDiscussionPage project_handle language target widget)

--------------------------------------------------------------------------------
-- /rethread

getRethreadWikiCommentR :: Text -> Language -> Text -> CommentId -> Handler Html
getRethreadWikiCommentR project_handle language target comment_id = do
    (widget, _) <-
        makeWikiPageCommentActionWidget
            makeRethreadCommentWidget
            project_handle
            language
            target
            comment_id
            def
            getMaxDepth

    defaultLayout (wikiDiscussionPage project_handle language target widget)

postRethreadWikiCommentR :: Text -> Language -> Text -> CommentId -> Handler Html
postRethreadWikiCommentR project_handle language target comment_id = do
    (user@(Entity user_id _), _, _, comment) <- checkCommentPageRequireAuth project_handle language target comment_id
    checkProjectCommentActionPermission can_rethread user project_handle (Entity comment_id comment)
    postRethreadComment user_id comment_id comment

--------------------------------------------------------------------------------
-- /retract

getRetractWikiCommentR :: Text -> Language -> Text -> CommentId -> Handler Html
getRetractWikiCommentR project_handle language target comment_id = do
    (widget, _) <-
        makeWikiPageCommentActionWidget
            makeRetractCommentWidget
            project_handle
            language
            target
            comment_id
            def
            getMaxDepth

    defaultLayout (wikiDiscussionPage project_handle language target widget)

postRetractWikiCommentR :: Text -> Language -> Text -> CommentId -> Handler Html
postRetractWikiCommentR project_handle language target comment_id = do
    (user, Entity project_id _, _, comment) <- checkCommentPageRequireAuth project_handle language target comment_id
    checkProjectCommentActionPermission can_retract user project_handle (Entity comment_id comment)

    postRetractComment
      user
      comment_id
      comment
      (wikiPageCommentHandlerInfo (Just user) project_id project_handle language target)
      >>= \case
        Nothing -> redirect (WikiCommentR project_handle language target comment_id)
        Just (widget, form) -> defaultLayout $ previewWidget form "retract" (wikiDiscussionPage project_handle language target widget)

--------------------------------------------------------------------------------
-- /tags

getWikiCommentTagsR :: Text -> Language -> Text -> CommentId -> Handler Html
getWikiCommentTagsR _ _ _ = getCommentTags

--------------------------------------------------------------------------------
-- /tag/#TagId

getWikiCommentTagR :: Text -> Language -> Text -> CommentId -> TagId -> Handler Html
getWikiCommentTagR _ _ _ = getCommentTagR

postWikiCommentTagR :: Text -> Language -> Text -> CommentId -> TagId -> Handler ()
postWikiCommentTagR _ _ _ = postCommentTagR

--------------------------------------------------------------------------------
-- /tag/apply, /tag/create

postWikiCommentApplyTagR, postWikiCommentCreateTagR :: Text -> Language -> Text -> CommentId -> Handler Html
postWikiCommentApplyTagR  = applyOrCreate postCommentApplyTag
postWikiCommentCreateTagR = applyOrCreate postCommentCreateTag

applyOrCreate :: (CommentId -> Handler ()) -> Text -> Language -> Text -> CommentId -> Handler Html
applyOrCreate action project_handle language target comment_id = do
    action comment_id
    redirect (WikiCommentR project_handle language target comment_id)

--------------------------------------------------------------------------------
-- /tag/new

getWikiCommentAddTagR :: Text -> Language -> Text -> CommentId -> Handler Html
getWikiCommentAddTagR project_handle language target comment_id = do
    (user@(Entity user_id _), Entity project_id _, _, comment) <- checkCommentPageRequireAuth project_handle language target comment_id
    checkProjectCommentActionPermission can_add_tag user project_handle (Entity comment_id comment)
    getProjectCommentAddTag comment_id project_id user_id


--------------------------------------------------------------------------------
-- /unclaim

getUnclaimWikiCommentR :: Text -> Language -> Text -> CommentId -> Handler Html
getUnclaimWikiCommentR project_handle language target comment_id = do
    (widget, _) <-
        makeWikiPageCommentActionWidget
            makeUnclaimCommentWidget
            project_handle
            language
            target
            comment_id
            def
            getMaxDepth

    defaultLayout (wikiDiscussionPage project_handle language target widget)

postUnclaimWikiCommentR :: Text -> Language -> Text -> CommentId -> Handler Html
postUnclaimWikiCommentR project_handle language target comment_id = do
    (user, Entity project_id _, _, comment) <- checkCommentPageRequireAuth project_handle language target comment_id
    checkProjectCommentActionPermission can_unclaim user project_handle (Entity comment_id comment)

    postUnclaimComment
      user
      comment_id
      comment
      (wikiPageCommentHandlerInfo (Just user) project_id project_handle language target)
      >>= \case
        Nothing -> redirect (WikiCommentR project_handle language target comment_id)
        Just (widget, form) -> defaultLayout $ previewWidget form "unclaim" (wikiDiscussionPage project_handle language target widget)

--------------------------------------------------------------------------------
-- /watch

getWatchWikiCommentR :: Text -> Language -> Text -> CommentId -> Handler Html
getWatchWikiCommentR project_handle language target comment_id = do
    (widget, _) <-
        makeWikiPageCommentActionWidget
            makeWatchCommentWidget
            project_handle
            language
            target
            comment_id
            def
            getMaxDepth

    defaultLayout (wikiDiscussionPage project_handle language target widget)

postWatchWikiCommentR :: Text -> Language -> Text -> CommentId -> Handler Html
postWatchWikiCommentR project_handle language target comment_id = do
    (viewer@(Entity viewer_id _), _, _, comment) <- checkCommentPageRequireAuth project_handle language target comment_id
    checkProjectCommentActionPermission can_watch viewer project_handle (Entity comment_id comment)

    postWatchComment viewer_id comment_id

    redirect (WikiCommentR project_handle language target comment_id)

--------------------------------------------------------------------------------
-- /unwatch

getUnwatchWikiCommentR :: Text -> Language -> Text -> CommentId -> Handler Html
getUnwatchWikiCommentR project_handle language target comment_id = do
    (widget, _) <-
        makeWikiPageCommentActionWidget
            makeUnwatchCommentWidget
            project_handle
            language
            target
            comment_id
            def
            getMaxDepth

    defaultLayout (wikiDiscussionPage project_handle language target widget)

postUnwatchWikiCommentR :: Text -> Language -> Text -> CommentId -> Handler Html
postUnwatchWikiCommentR project_handle language target comment_id = do
    (viewer@(Entity viewer_id _), _, _, comment) <- checkCommentPageRequireAuth project_handle language target comment_id
    checkProjectCommentActionPermission can_unwatch viewer project_handle (Entity comment_id comment)

    postUnwatchComment viewer_id comment_id

    redirect (WikiCommentR project_handle language target comment_id)

--------------------------------------------------------------------------------
-- DEPRECATED

-- This is just because we used to have "/comment/#" with that longer URL,
-- and this keeps any permalinks from breaking
getOldDiscussCommentR :: Text -> Language -> Text -> CommentId -> Handler Html
getOldDiscussCommentR project_handle language target comment_id = redirect $ WikiCommentR project_handle language target comment_id
