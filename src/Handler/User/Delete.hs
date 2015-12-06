module Handler.User.Delete where

import Import

import Handler.Utils
import Handler.User.Utils (checkEditUser)
import Model.User

startDeleteConfirmation :: UserId -> Handler ()
startDeleteConfirmation user_id = do
    hash        <- liftIO newHash
    confirm_uri <- getUrlRender <*> (pure $ UserConfirmDeleteR user_id hash)
    muser_email <- runDB $ fetchUserEmailVerified user_id
    case muser_email of
        Nothing -> alertDanger $
            "Cannot continue without a verified email address. " <>
            "Please add one to your profile and verify it."
        Just user_email -> do
            runDB $ insert_ $
                DeleteConfirmation user_id user_email confirm_uri False
            alertSuccess $
                "Confirmation email has been sent to " <> user_email <> "."

getDeleteUserR :: UserId -> Handler Html
getDeleteUserR user_id = do
    void $ checkEditUser user_id
    user <- runYDB $ get404 user_id
    defaultLayout $ do
        snowdriftDashTitle "Delete Account" $
            userDisplayName (Entity user_id user)
        $(widgetFile "delete_user")

postDeleteUserR :: UserId -> Handler Html
postDeleteUserR user_id = do
    void $ checkEditUser user_id
    startDeleteConfirmation user_id
    redirect $ UserR user_id
