-- | Handler for comments on Wiki pages. Section comments are relative to /p/#handle/w/#target/c/#comment

module Handler.Wiki.Comment where

import Import

import           Handler.Comment
import           Model.Comment
import           Model.Comment.ActionPermissions
import           Model.Comment.HandlerInfo
import           Model.Comment.Sql
import           Model.User

import           Data.Default                    (def)
import           Data.Tree                       (Forest, Tree)
import qualified Data.Tree                       as Tree

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
        let has_permission = exprCommentProjectPermissionFilter muser_id (val project_id)
        ecomment <- fetchCommentDB comment_id has_permission
        return (project, page, ecomment)

    case ecomment of
        Left CommentNotFound         -> notFound
        Left CommentPermissionDenied -> permissionDenied "You don't have permission to view this comment."
        Right comment                -> return (project, page, comment)

checkWikiPageCommentActionPermission
        :: (CommentActionPermissions -> Bool)
        -> Text
        -> Entity Comment
        -> Handler ()
checkWikiPageCommentActionPermission can_perform_action project_handle comment = do
    ok <- can_perform_action <$> makeProjectCommentActionPermissions project_handle comment
    unless ok (permissionDenied "You don't have permission to perform this action.")

makeWikiPageCommentForestWidget
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
makeWikiPageCommentForestWidget
        muser
        project_id
        project_handle
        target
        comments
        comment_mods
        get_max_depth
        is_preview
        widget_under_root_comment = do
    makeCommentForestWidget
      (wikiPageCommentHandlerInfo (entityKey <$> muser) project_id project_handle target)
      comments
      muser
      comment_mods
      get_max_depth
      is_preview
      widget_under_root_comment

makeWikiPageCommentTreeWidget
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
makeWikiPageCommentTreeWidget a b c d e f g h i = do
    (widget, [tree]) <- makeWikiPageCommentForestWidget a b c d [e] f g h i
    return (widget, tree)

makeWikiPageCommentActionWidget
        :: MakeCommentActionWidget
        -> Text
        -> Text
        -> CommentId
        -> CommentMods
        -> Handler MaxDepth
        -> Handler (Widget, Tree (Entity Comment))
makeWikiPageCommentActionWidget make_comment_action_widget project_handle target comment_id mods get_max_depth = do
    (user@(Entity user_id _), Entity project_id _, _, comment) <- checkCommentPageRequireAuth project_handle target comment_id
    make_comment_action_widget
      (Entity comment_id comment)
      user
      (wikiPageCommentHandlerInfo (Just user_id) project_id project_handle target)
      mods
      get_max_depth
      False

--------------------------------------------------------------------------------
-- /

getWikiCommentR :: Text -> Text -> CommentId -> Handler Html
getWikiCommentR project_handle target comment_id = do
    (muser, Entity project_id _, _, comment) <- checkCommentPage project_handle target comment_id
    (widget, comment_tree) <-
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
        Just (Entity user_id _) ->
            runDB (userMaybeViewProjectCommentsDB user_id project_id (map entityKey (Tree.flatten comment_tree)))

    defaultLayout $(widgetFile "wiki_discussion_wrapper")

--------------------------------------------------------------------------------
-- /close

getCloseWikiCommentR :: Text -> Text -> CommentId -> Handler Html
getCloseWikiCommentR project_handle target comment_id = do
    (widget, _) <-
        makeWikiPageCommentActionWidget
          makeCloseCommentWidget
          project_handle
          target
          comment_id
          def
          getMaxDepth
    defaultLayout $(widgetFile "wiki_discussion_wrapper")

postCloseWikiCommentR :: Text -> Text -> CommentId -> Handler Html
postCloseWikiCommentR project_handle target comment_id = do
    (user@(Entity user_id _), (Entity project_id _), _, comment) <- checkCommentPageRequireAuth project_handle target comment_id
    checkWikiPageCommentActionPermission can_close project_handle (Entity comment_id comment)

    postCloseComment
      user
      comment_id
      comment
      (wikiPageCommentHandlerInfo (Just user_id) project_id project_handle target)
      >>= \case
        Nothing -> redirect (WikiCommentR project_handle target comment_id)  -- Closure posted.
        Just widget -> defaultLayout $(widgetFile "wiki_discussion_wrapper") -- Previewing closure.

--------------------------------------------------------------------------------
-- /delete

getDeleteWikiCommentR :: Text -> Text -> CommentId -> Handler Html
getDeleteWikiCommentR project_handle target comment_id = do
    (widget, _) <-
        makeWikiPageCommentActionWidget
          makeDeleteCommentWidget
          project_handle
          target
          comment_id
          def
          (getMaxDepthDefault 0)
    defaultLayout $(widgetFile "wiki_discussion_wrapper")

postDeleteWikiCommentR :: Text -> Text -> CommentId -> Handler Html
postDeleteWikiCommentR project_handle target comment_id = do
    (_, _, _, comment) <- checkCommentPage project_handle target comment_id
    checkWikiPageCommentActionPermission can_delete project_handle (Entity comment_id comment)

    was_deleted <- postDeleteComment comment_id
    if was_deleted
        then redirect (WikiDiscussionR project_handle target)
        else redirect (WikiCommentR project_handle target comment_id)

--------------------------------------------------------------------------------
-- /edit

getEditWikiCommentR :: Text -> Text -> CommentId -> Handler Html
getEditWikiCommentR project_handle target comment_id = do
    (widget, _) <-
        makeWikiPageCommentActionWidget
          makeEditCommentWidget
          project_handle
          target
          comment_id
          def
          (getMaxDepthDefault 0)
    defaultLayout $(widgetFile "wiki_discussion_wrapper")

postEditWikiCommentR :: Text -> Text -> CommentId -> Handler Html
postEditWikiCommentR project_handle target comment_id = do
    (user@(Entity user_id _), Entity project_id _, _, comment) <- checkCommentPageRequireAuth project_handle target comment_id
    checkWikiPageCommentActionPermission can_edit project_handle (Entity comment_id comment)

    postEditComment
      user
      (Entity comment_id comment)
      (wikiPageCommentHandlerInfo (Just user_id) project_id project_handle target)
      >>= \case
        Nothing -> redirect (WikiCommentR project_handle target comment_id)  -- Edit made.
        Just widget -> defaultLayout $(widgetFile "wiki_discussion_wrapper") -- Previewing edit.

--------------------------------------------------------------------------------
-- /flag

getFlagWikiCommentR :: Text -> Text -> CommentId -> Handler Html
getFlagWikiCommentR project_handle target comment_id = do
    (widget, _) <-
        makeWikiPageCommentActionWidget
          makeFlagCommentWidget
          project_handle
          target
          comment_id
          def
          (getMaxDepthDefault 0)
    defaultLayout $(widgetFile "wiki_discussion_wrapper")

postFlagWikiCommentR :: Text -> Text -> CommentId -> Handler Html
postFlagWikiCommentR project_handle target comment_id = do
    (user@(Entity user_id _), Entity project_id _, _, comment) <- checkCommentPageRequireAuth project_handle target comment_id
    checkWikiPageCommentActionPermission can_flag project_handle (Entity comment_id comment)

    postFlagComment
      user
      (Entity comment_id comment)
      (wikiPageCommentHandlerInfo (Just user_id) project_id project_handle target)
      >>= \case
        Nothing -> redirect (WikiDiscussionR project_handle target)
        Just widget -> defaultLayout $(widgetFile "wiki_discussion_wrapper")

--------------------------------------------------------------------------------
-- /moderate TODO: rename to /approve

getApproveWikiCommentR :: Text -> Text -> CommentId -> Handler Html
getApproveWikiCommentR project_handle target comment_id = do
    (widget, _) <-
        makeWikiPageCommentActionWidget
          makeApproveCommentWidget
          project_handle
          target
          comment_id
          def
          getMaxDepth
    defaultLayout $(widgetFile "wiki_discussion_wrapper")

postApproveWikiCommentR :: Text -> Text -> CommentId -> Handler Html
postApproveWikiCommentR project_handle target comment_id = do
    (Entity user_id _, _, _, comment) <- checkCommentPageRequireAuth project_handle target comment_id
    checkWikiPageCommentActionPermission can_approve project_handle (Entity comment_id comment)

    postApproveComment user_id comment_id comment
    redirect (WikiCommentR project_handle target comment_id)

--------------------------------------------------------------------------------
-- /reply

getReplyWikiCommentR :: Text -> Text -> CommentId -> Handler Html
getReplyWikiCommentR project_handle target comment_id = do
    (widget, _) <- makeWikiPageCommentActionWidget
                     makeReplyCommentWidget
                     project_handle
                     target
                     comment_id
                     def
                     getMaxDepth
    defaultLayout $(widgetFile "wiki_discussion_wrapper")

postReplyWikiCommentR :: Text -> Text -> CommentId -> Handler Html
postReplyWikiCommentR project_handle target parent_id = do
    (Entity user_id user, _, Entity _ page, parent) <- checkCommentPageRequireAuth project_handle target parent_id
    checkWikiPageCommentActionPermission can_reply project_handle (Entity parent_id parent)

    postNewComment
      (Just parent_id)
      user_id
      (userIsEstablished user)
      (wikiPageDiscussion page)
      (makeProjectCommentActionPermissions project_handle) >>= \case
        Left _ -> redirect (WikiCommentR project_handle target parent_id)     -- Posted the reply.
        Right widget -> defaultLayout $(widgetFile "wiki_discussion_wrapper") -- Previewing the reply.

--------------------------------------------------------------------------------
-- /rethread

getRethreadWikiCommentR :: Text -> Text -> CommentId -> Handler Html
getRethreadWikiCommentR project_handle target comment_id = do
    (widget, _) <-
        makeWikiPageCommentActionWidget
          makeRethreadCommentWidget
          project_handle
          target
          comment_id
          def
          (getMaxDepthDefault 0)
    defaultLayout $(widgetFile "wiki_discussion_wrapper")

postRethreadWikiCommentR :: Text -> Text -> CommentId -> Handler Html
postRethreadWikiCommentR project_handle target comment_id = do
    (Entity user_id _, _, _, comment) <- checkCommentPageRequireAuth project_handle target comment_id
    checkWikiPageCommentActionPermission can_rethread project_handle (Entity comment_id comment)
    postRethreadComment user_id comment_id comment

--------------------------------------------------------------------------------
-- /retract

getRetractWikiCommentR :: Text -> Text -> CommentId -> Handler Html
getRetractWikiCommentR project_handle target comment_id = do
    (widget, _) <-
        makeWikiPageCommentActionWidget
          makeRetractCommentWidget
          project_handle
          target
          comment_id
          def
          getMaxDepth
    defaultLayout $(widgetFile "wiki_discussion_wrapper")

postRetractWikiCommentR :: Text -> Text -> CommentId -> Handler Html
postRetractWikiCommentR project_handle target comment_id = do
    (user@(Entity user_id _), Entity project_id _, _, comment) <- checkCommentPageRequireAuth project_handle target comment_id
    checkWikiPageCommentActionPermission can_retract project_handle (Entity comment_id comment)

    postRetractComment
      user
      comment_id
      comment
      (wikiPageCommentHandlerInfo (Just user_id) project_id project_handle target)
      >>= \case
        Nothing -> redirect (WikiCommentR project_handle target comment_id)  -- Closure posted.
        Just widget -> defaultLayout $(widgetFile "wiki_discussion_wrapper") -- Previewing closure.

--------------------------------------------------------------------------------
-- tag passthroughs, for better looking URLs

getWikiCommentTagsR :: Text -> Text -> CommentId -> Handler Html
getWikiCommentTagsR _ _ = getCommentTagsR

getWikiCommentTagR :: Text -> Text -> CommentId -> TagId -> Handler Html
getWikiCommentTagR _ _ = getCommentTagR

postWikiCommentTagR :: Text -> Text -> CommentId -> TagId -> Handler Html
postWikiCommentTagR _ _ = postCommentTagR

postWikiCommentApplyTagR :: Text -> Text -> CommentId -> Handler Html
postWikiCommentApplyTagR  _ _ = postCommentApplyTagR

postWikiCommentCreateTagR :: Text -> Text -> CommentId -> Handler Html
postWikiCommentCreateTagR _ _ = postCommentCreateTagR

--------------------------------------------------------------------------------
-- /tag/new

getWikiCommentAddTagR :: Text -> Text -> CommentId -> Handler Html
getWikiCommentAddTagR project_handle target comment_id = do
    (Entity user_id _, Entity project_id _, _, comment) <- checkCommentPageRequireAuth project_handle target comment_id
    ok <- can_add_tag <$> makeProjectCommentActionPermissions project_handle (Entity comment_id comment)
    unless ok (permissionDenied "You don't have permission to view this page.")
    getProjectCommentAddTag comment_id project_id user_id

--------------------------------------------------------------------------------
-- DEPRECATED

-- This is just because we used to have "/comment/#" with that longer URL,
-- and this keeps any permalinks from breaking
getOldDiscussCommentR :: Text -> Text -> CommentId -> Handler Html
getOldDiscussCommentR project_handle target comment_id = redirect $ WikiCommentR project_handle target comment_id
