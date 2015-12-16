module Handler.User.Discussion where

import Import

import Text.Cassius (cassiusFile)
import qualified Data.Default as Default

import Handler.Comment as Com
import Handler.User.Comment as Com
import Handler.Discussion
import Handler.Utils
import Model.Comment.Sql
import Model.User
import View.Comment

-- | getUserDiscussionR generates the associated discussion page for each user
getUserDiscussionR :: UserId -> Handler Html
getUserDiscussionR user_id = getDiscussion (getUserDiscussionR' user_id)

getUserDiscussionR'
        :: UserId
        -> (DiscussionId -> ExprCommentCond -> DB [Entity Comment])  -- ^ Root comment getter.
        -> Handler Html
getUserDiscussionR' user_id get_root_comments = do
    mviewer <- maybeAuth
    let mviewer_id = entityKey <$> mviewer

    (user, root_comments) <- runYDB $ do
        user <- get404 user_id
        let has_permission = (exprCommentUserPermissionFilter mviewer_id (val user_id))
        root_comments <- get_root_comments (userDiscussion user) has_permission
        return (user, root_comments)

    maxDepth <- getMaxDepth
    (comment_forest_no_css, _) <-
        makeUserCommentForestWidget
            mviewer
            user_id
            root_comments
            Default.def
            maxDepth
            False
            mempty

    let has_comments = not (null root_comments)
        comment_forest = do
            comment_forest_no_css
            toWidget $(cassiusFile "templates/comment.cassius")

    (comment_form, _) <- generateFormPost commentNewTopicForm

    defaultLayout $ do
        snowdriftTitle $
            userDisplayName (Entity user_id user) <>
            " User Discussion"
        $(widgetFile "user_discuss")

postUserDiscussionR :: UserId -> Handler Html
postUserDiscussionR _ = error "TODO(mitchell)"
