module Handler.User.SelectProject where

import Import

import Data.List (head)
import qualified Data.Text as T
import qualified Data.Traversable as Traversable

import Handler.Utils
import Handler.User.Utils (checkEditUser)
import Model.User

-- /#UserId/select-project

getUserSelectProjectR :: UserId -> Handler Html
getUserSelectProjectR user_id = do
    void $ checkEditUser user_id
    user <- runYDB $ get404 user_id
    projects <- runDB $ fetchUserWatchingProjectsDB user_id
    if length projects == 1
        then redirect $ ProjectNotificationsR user_id $ entityKey $ head projects
        else defaultLayout $ do
            snowdriftDashTitle "Select Project" $
                userDisplayName (Entity user_id user)
            $(widgetFile "user_select_project")

postUserSelectProjectR :: UserId -> Handler Html
postUserSelectProjectR user_id = do
    void $ checkEditUser user_id
    mproject_id <- lookupPostParam "project_id"
    maybe (redirect $ UserR user_id)
          (redirect . ProjectNotificationsR user_id . key . PersistInt64)
          (join $ Traversable.forM mproject_id $ readMaybe . T.unpack)
