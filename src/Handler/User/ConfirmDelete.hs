module Handler.User.ConfirmDelete where

import Import

import Handler.Utils
import Handler.User.Utils (checkEditUser)
import Model.User


checkConfirmDelete :: UserId -> Text -> Handler User
checkConfirmDelete user_id hash = do
    confirm_uri <- getUrlRender <*> pure (UserConfirmDeleteR user_id hash)
    muser_email <- runDB $ fetchUserEmail user_id
    case muser_email of
        Nothing    -> notFound
        Just email -> runYDB $ do
            -- Check whether the hash is in the DB.
            void $ getBy404 $ UniqueDeleteConfirmation user_id email confirm_uri
            get404 user_id

getUserConfirmDeleteR :: UserId -> Text -> Handler Html
getUserConfirmDeleteR user_id hash = do
    void $ checkEditUser user_id
    user <- checkConfirmDelete user_id hash
    defaultLayout $ do
        snowdriftDashTitle "Delete Account" $
            userDisplayName (Entity user_id user)
        $(widgetFile "user_confirm_delete")

postUserConfirmDeleteR :: UserId -> Text -> Handler Html
postUserConfirmDeleteR user_id hash = do
    void $ checkEditUser user_id
    void $ checkConfirmDelete user_id hash
    runDB $ deleteUserDB user_id
    alertSuccess "Successfully deleted your account."
    redirect HomeR
