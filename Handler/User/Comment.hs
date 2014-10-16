-- | Handler for comments on User pages. Section comments are relative to /p/#handle/w/#target/c/#comment

module Handler.User.Comment where

import Import

import           Handler.Comment
import           Model.Comment
import           Model.Comment.ActionPermissions
import           Model.Comment.HandlerInfo
import           Model.Comment.Mods
import           Widgets.Preview

import           Data.Default                    (def)
import           Data.Tree                       (Forest, Tree)
import           Text.Cassius (cassiusFile)

--------------------------------------------------------------------------------
-- Utility functions

-- | Convenience method for all pages that accept a user id and comment id
-- as URL parameters. Makes sure that the comment is indeed on the page. Redirects if the
-- comment was rethreaded. 404's if the comment doesn't exist. 403 if permission denied.
checkCommentUrl :: UserId -> CommentId -> Handler (Maybe (Entity User), Comment)
checkCommentUrl user_id comment_id = do
    mviewer <- maybeAuth
    comment <- checkCommentUrl' (entityKey <$> mviewer) user_id comment_id
    return (mviewer, comment)

-- | Like checkCommentUrl, but authentication is required.
checkCommentUrlRequireAuth :: UserId -> CommentId -> Handler (Entity User, Comment)
checkCommentUrlRequireAuth user_id comment_id = do
    viewer@(Entity viewer_id _) <- requireAuth
    comment <- checkCommentUrl' (Just viewer_id) user_id comment_id
    return (viewer, comment)

-- | Abstract checkCommentUrl and checkCommentUrlRequireAuth. You shouldn't
-- use this function directly.
checkCommentUrl' :: Maybe UserId -> UserId -> CommentId -> Handler Comment
checkCommentUrl' mviewer_id user_id comment_id = do
    redirectIfRethreaded comment_id

    (user, ecomment) <- runYDB $ do
        user <- get404 user_id

        let has_permission :: SqlExpr (Entity Comment) -> SqlExpr (Value Bool)
            has_permission = if mviewer_id == Just user_id
                then \ _ -> val True
                else \ c -> c ^. CommentVisibility ==. val VisPublic
                        ||. just (c ^. CommentUser) ==. val mviewer_id
                        ||. ( c ^. CommentId `in_`
                                ( subList_select $ from $ \ (ca `InnerJoin` c2) -> do
                                    on_ $ ca ^. CommentAncestorAncestor ==. c2 ^. CommentId
                                    where_ $ just (c2 ^. CommentUser) ==. val mviewer_id

                                    return $ ca ^. CommentAncestorComment
                                )  -- TODO - I think we wanted this a tiny bit more restrictive
                            )

        ecomment <- fetchCommentDB comment_id has_permission
        return (user, ecomment)

    case ecomment of
        Left CommentNotFound         -> notFound
        Left CommentPermissionDenied -> permissionDenied "You don't have permission to view this comment."
        Right comment                ->
            if commentDiscussion comment /= userDiscussion user
                then notFound
                else return comment


checkUserCommentActionPermission
        :: (CommentActionPermissions -> Bool)
        -> Entity User
        -> UserId
        -> Entity Comment
        -> Handler ()
checkUserCommentActionPermission
        can_perform_action
        user
        user_id
        comment@(Entity comment_id _) = do
    action_permissions <-
        lookupErr "checkUserCommentActionPermission: comment id not found in map" comment_id
          <$> makeUserCommentActionPermissionsMap (Just user) user_id def [comment]
    unless (can_perform_action action_permissions)
           (permissionDenied "You don't have permission to perform this action.")


makeUserCommentForestWidget
        :: Maybe (Entity User)
        -> UserId
        -> [Entity Comment]
        -> CommentMods
        -> Handler MaxDepth
        -> Bool
        -> Widget
        -> Handler (Widget, Forest (Entity Comment))
makeUserCommentForestWidget
        muser
        user_id
        comments
        comment_mods
        get_max_depth
        is_preview
        widget_under_root_comment = do
    makeCommentForestWidget
      (userCommentHandlerInfo muser user_id)
      comments
      muser
      comment_mods
      get_max_depth
      is_preview
      widget_under_root_comment

makeUserCommentTreeWidget
        :: Maybe (Entity User)
        -> UserId
        -> Entity Comment
        -> CommentMods
        -> Handler MaxDepth
        -> Bool
        -> Widget
        -> Handler (Widget, Tree (Entity Comment))
makeUserCommentTreeWidget a b c d e f g = do
    (widget, [tree]) <- makeUserCommentForestWidget a b [c] d e f g
    return (widget, tree)

makeUserCommentActionWidget
        :: MakeCommentActionWidget
        -> UserId
        -> CommentId
        -> CommentMods
        -> Handler MaxDepth
        -> Handler (Widget, Tree (Entity Comment))
makeUserCommentActionWidget make_comment_action_widget user_id comment_id mods get_max_depth = do
    (user, comment) <- checkCommentUrlRequireAuth user_id comment_id
    make_comment_action_widget
      (Entity comment_id comment)
      user
      (userCommentHandlerInfo (Just user) user_id)
      mods
      get_max_depth
      False

userDiscussionPage :: UserId -> Widget -> Widget
userDiscussionPage user_id widget = do
    $(widgetFile "user_discussion_wrapper")
    toWidget $(cassiusFile "templates/comment.cassius")

--------------------------------------------------------------------------------
-- /

getUserCommentR :: UserId -> CommentId -> Handler Html
getUserCommentR user_id comment_id = do
    (muser, comment) <- checkCommentUrl user_id comment_id
    (widget, _) <-
        makeUserCommentTreeWidget
          muser
          user_id
          (Entity comment_id comment)
          def
          getMaxDepth
          False
          mempty

    {- TODO: watch user discussion?
    case muser of
        Nothing -> return ()
        Just (Entity user_id _) ->
            runDB (userMaybeViewUserCommentsDB user_id (map entityKey (Tree.flatten comment_tree)))
    -}

    defaultLayout (userDiscussionPage user_id widget)

--------------------------------------------------------------------------------
-- /claim

getClaimUserCommentR :: UserId -> CommentId -> Handler Html
getClaimUserCommentR user_id comment_id = do
    (widget, _) <-
        makeUserCommentActionWidget
          makeClaimCommentWidget
          user_id
          comment_id
          def
          getMaxDepth

    defaultLayout (userDiscussionPage user_id widget)

postClaimUserCommentR :: UserId -> CommentId -> Handler Html
postClaimUserCommentR user_id comment_id = do
    (viewer, comment) <- checkCommentUrlRequireAuth user_id comment_id
    checkUserCommentActionPermission can_claim viewer user_id (Entity comment_id comment)

    postClaimComment
      viewer
      comment_id
      comment
      (userCommentHandlerInfo (Just viewer) user_id)
      >>= \case
        Nothing -> redirect (UserCommentR user_id comment_id)
        Just (widget, form) -> defaultLayout $ previewWidget form "claim" (userDiscussionPage user_id widget)

--------------------------------------------------------------------------------
-- /approve

getApproveUserCommentR :: UserId -> CommentId -> Handler Html
getApproveUserCommentR user_id comment_id = do
    (widget, _) <-
        makeUserCommentActionWidget
          makeApproveCommentWidget
          user_id
          comment_id
          def
          getMaxDepth
    defaultLayout (userDiscussionPage user_id widget)

postApproveUserCommentR :: UserId -> CommentId -> Handler Html
postApproveUserCommentR user_id comment_id = do
    (viewer, comment) <- checkCommentUrlRequireAuth user_id comment_id
    checkUserCommentActionPermission can_approve viewer user_id (Entity comment_id comment)

    postApproveComment user_id comment_id comment
    redirect (UserCommentR user_id comment_id)

--------------------------------------------------------------------------------
-- /close

getCloseUserCommentR :: UserId -> CommentId -> Handler Html
getCloseUserCommentR user_id comment_id = do
    (widget, _) <-
        makeUserCommentActionWidget
          makeCloseCommentWidget
          user_id
          comment_id
          def
          getMaxDepth
    defaultLayout (userDiscussionPage user_id widget)

postCloseUserCommentR :: UserId -> CommentId -> Handler Html
postCloseUserCommentR user_id comment_id = do
    (viewer, comment) <- checkCommentUrlRequireAuth user_id comment_id
    checkUserCommentActionPermission can_close viewer user_id (Entity comment_id comment)

    postCloseComment
      viewer
      comment_id
      comment
      (userCommentHandlerInfo (Just viewer) user_id)
      >>= \case
        Nothing -> redirect (UserCommentR user_id comment_id)
        Just (widget, form) -> defaultLayout $ previewWidget form "close" (userDiscussionPage user_id widget)

--------------------------------------------------------------------------------
-- /delete

getDeleteUserCommentR :: UserId -> CommentId -> Handler Html
getDeleteUserCommentR user_id comment_id = do
    (widget, _) <-
        makeUserCommentActionWidget
          makeDeleteCommentWidget
          user_id
          comment_id
          def
          getMaxDepth
    defaultLayout (userDiscussionPage user_id widget)

postDeleteUserCommentR :: UserId -> CommentId -> Handler Html
postDeleteUserCommentR user_id comment_id = do
    (viewer, comment) <- checkCommentUrlRequireAuth user_id comment_id
    checkUserCommentActionPermission can_delete viewer user_id (Entity comment_id comment)

    was_deleted <- postDeleteComment comment_id
    if was_deleted
        then redirect (UserDiscussionR user_id)
        else redirect (UserCommentR user_id comment_id)

--------------------------------------------------------------------------------
-- /edit

getEditUserCommentR :: UserId -> CommentId -> Handler Html
getEditUserCommentR user_id comment_id = do
    (widget, _) <-
        makeUserCommentActionWidget
          makeEditCommentWidget
          user_id
          comment_id
          def
          getMaxDepth
    defaultLayout (userDiscussionPage user_id widget)

postEditUserCommentR :: UserId -> CommentId -> Handler Html
postEditUserCommentR user_id comment_id = do
    (viewer, comment) <- checkCommentUrlRequireAuth user_id comment_id
    checkUserCommentActionPermission can_edit viewer user_id (Entity comment_id comment)

    postEditComment
      viewer
      (Entity comment_id comment)
      (userCommentHandlerInfo (Just viewer) user_id)
      >>= \case
        Nothing -> redirect (UserCommentR user_id comment_id)  -- Edit made.
        Just (widget, form) -> defaultLayout $ previewWidget form "post" (userDiscussionPage user_id widget)

--------------------------------------------------------------------------------
-- /flag

getFlagUserCommentR :: UserId -> CommentId -> Handler Html
getFlagUserCommentR user_id comment_id = do
    (widget, _) <-
        makeUserCommentActionWidget
          makeFlagCommentWidget
          user_id
          comment_id
          def
          getMaxDepth
    defaultLayout (userDiscussionPage user_id widget)

postFlagUserCommentR :: UserId -> CommentId -> Handler Html
postFlagUserCommentR user_id comment_id = do
    (viewer, comment) <- checkCommentUrlRequireAuth user_id comment_id
    checkUserCommentActionPermission can_flag viewer user_id (Entity comment_id comment)

    postFlagComment
      viewer
      (Entity comment_id comment)
      (userCommentHandlerInfo (Just viewer) user_id)
      >>= \case
        Nothing -> redirect (UserDiscussionR user_id)
        Just (widget, form) -> defaultLayout $ previewWidget form "flag" (userDiscussionPage user_id widget)

--------------------------------------------------------------------------------
-- /reply

getReplyUserCommentR :: UserId -> CommentId -> Handler Html
getReplyUserCommentR user_id comment_id = do
    (widget, _) <-
        makeUserCommentActionWidget
          makeReplyCommentWidget
          user_id
          comment_id
          def
          getMaxDepth
    defaultLayout (userDiscussionPage user_id widget)

postReplyUserCommentR :: UserId -> CommentId -> Handler Html
postReplyUserCommentR user_id parent_id = do
    (viewer, parent) <- checkCommentUrlRequireAuth user_id parent_id
    checkUserCommentActionPermission can_reply viewer user_id (Entity parent_id parent)

    user <- runYDB $ get404 user_id

    postNewComment
      (Just parent_id)
      viewer
      (userDiscussion user)
      (makeUserCommentActionPermissionsMap (Just viewer) user_id def) >>= \case
        Left _ -> redirect (UserCommentR user_id parent_id)
        Right (widget, form) -> defaultLayout $ previewWidget form "post" (userDiscussionPage user_id widget)

--------------------------------------------------------------------------------
-- /rethread

getRethreadUserCommentR :: UserId -> CommentId -> Handler Html
getRethreadUserCommentR user_id comment_id = do
    (widget, _) <-
        makeUserCommentActionWidget
          makeRethreadCommentWidget
          user_id
          comment_id
          def
          getMaxDepth
    defaultLayout (userDiscussionPage user_id widget)

postRethreadUserCommentR :: UserId -> CommentId -> Handler Html
postRethreadUserCommentR user_id comment_id = do
    (viewer, comment) <- checkCommentUrlRequireAuth user_id comment_id
    checkUserCommentActionPermission can_rethread viewer user_id (Entity comment_id comment)
    postRethreadComment user_id comment_id comment

--------------------------------------------------------------------------------
-- /retract

getRetractUserCommentR :: UserId -> CommentId -> Handler Html
getRetractUserCommentR user_id comment_id = do
    (widget, _) <-
        makeUserCommentActionWidget
          makeRetractCommentWidget
          user_id
          comment_id
          def
          getMaxDepth
    defaultLayout (userDiscussionPage user_id widget)

postRetractUserCommentR :: UserId -> CommentId -> Handler Html
postRetractUserCommentR user_id comment_id = do
    (viewer, comment) <- checkCommentUrlRequireAuth user_id comment_id
    checkUserCommentActionPermission can_retract viewer user_id (Entity comment_id comment)

    postRetractComment
      viewer
      comment_id
      comment
      (userCommentHandlerInfo (Just viewer) user_id)
      >>= \case
        Nothing -> redirect (UserCommentR user_id comment_id)
        Just (widget, form) -> defaultLayout $ previewWidget form "retract" (userDiscussionPage user_id widget)

--------------------------------------------------------------------------------
-- /tags

getUserCommentTagsR :: UserId -> CommentId -> Handler Html
getUserCommentTagsR _ = getCommentTags

--------------------------------------------------------------------------------
-- /tag/#TagId

getUserCommentTagR :: UserId -> CommentId -> TagId -> Handler Html
getUserCommentTagR _ = getCommentTagR

postUserCommentTagR :: UserId -> CommentId -> TagId -> Handler ()
postUserCommentTagR _ = postCommentTagR

--------------------------------------------------------------------------------
-- /tag/apply, /tag/create

postUserCommentApplyTagR, postUserCommentCreateTagR :: UserId -> CommentId -> Handler Html
postUserCommentApplyTagR  = applyOrCreate postCommentApplyTag
postUserCommentCreateTagR = applyOrCreate postCommentCreateTag

applyOrCreate :: (CommentId -> Handler ()) -> UserId -> CommentId -> Handler Html
applyOrCreate action user_id comment_id = do
    action comment_id
    redirect (UserCommentR user_id comment_id)

--------------------------------------------------------------------------------
-- /tag/new

getUserCommentAddTagR :: UserId -> CommentId -> Handler Html
getUserCommentAddTagR user_id comment_id = do
    (viewer, comment) <- checkCommentUrlRequireAuth user_id comment_id
    checkUserCommentActionPermission can_add_tag viewer user_id (Entity comment_id comment)
    getUserCommentAddTag comment_id user_id


--------------------------------------------------------------------------------
-- /unclaim

getUnclaimUserCommentR :: UserId -> CommentId -> Handler Html
getUnclaimUserCommentR user_id comment_id = do
    (widget, _) <-
        makeUserCommentActionWidget
          makeUnclaimCommentWidget
          user_id
          comment_id
          def
          getMaxDepth

    defaultLayout (userDiscussionPage user_id widget)

postUnclaimUserCommentR :: UserId -> CommentId -> Handler Html
postUnclaimUserCommentR user_id comment_id = do
    (viewer, comment) <- checkCommentUrlRequireAuth user_id comment_id
    checkUserCommentActionPermission can_unclaim viewer user_id (Entity comment_id comment)

    postUnclaimComment
      viewer
      comment_id
      comment
      (userCommentHandlerInfo (Just viewer) user_id)
      >>= \case
        Nothing -> redirect (UserCommentR user_id comment_id)
        Just (widget, form) -> defaultLayout $ previewWidget form "unclaim" (userDiscussionPage user_id widget)

