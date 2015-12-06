module Handler.User.NewDiscussion where

import Import

import qualified Data.Default as Default

import Handler.Comment as Com
import Handler.User.Comment
import Model.Comment.ActionPermissions
import View.Comment
import Widgets.Preview

getNewUserDiscussionR :: UserId -> Handler Html
getNewUserDiscussionR user_id = do
    void requireAuth
    let widget = commentNewTopicFormWidget
    defaultLayout $(widgetFile "user_discussion_wrapper")

postNewUserDiscussionR :: UserId -> Handler Html
postNewUserDiscussionR user_id = do
    viewer <- requireAuth
    User{..} <- runYDB $ get404 user_id

    postNewComment
      Nothing
      viewer
      userDiscussion
      (makeUserCommentActionPermissionsMap (Just viewer) user_id Default.def) >>= \case
           ConfirmedPost (Left err) -> do
               alertDanger err
               redirect $ NewUserDiscussionR user_id
           ConfirmedPost (Right comment_id) ->
               redirect $ UserCommentR user_id comment_id
           Com.Preview (widget, form) ->
               defaultLayout $ previewWidget form "post" $
                   userDiscussionPage user_id widget
