module Handler.User.Edit where

import Import

import Data.Maybe (fromJust)

import Dev
import Handler.Utils
import Handler.User.Utils (checkEditUser, startEmailVerification)
import Model.User
import View.User
import Widgets.Preview


-- /#UserId/edit

getEditUserR :: UserId -> Handler Html
getEditUserR user_id = do
    _ <- checkEditUser user_id
    user <- runYDB (get404 user_id)

    (form, enctype) <- generateFormPost $ editUserForm (Just user)
    defaultLayout $ do
        snowdriftDashTitle "User Profile" $
            userDisplayName (Entity user_id user)
        $(widgetFile "edit_user")

postEditUserR :: UserId -> Handler Html
postEditUserR user_id = do
    viewer_id <- checkEditUser user_id

    ((result, _), _) <- runFormPost $ editUserForm Nothing

    case result of
        FormSuccess user_update ->
            lookupPostMode >>= \case
                Just PostMode -> do
                    let muser_email = userUpdateEmail user_update
                    when (isJust muser_email) $ do
                        let user_email = fromJust muser_email
                        mcurrent_email <- runDB $ fetchUserEmail user_id
                        when (mcurrent_email /= Just user_email) $
                            startEmailVerification user_id user_email
                    runDB (updateUserDB user_id user_update)
                    redirect (UserR user_id)

                _ -> do
                    user <- runYDB $ get404 user_id

                    let updated_user = updateUserPreview user_update user

                    (form, _) <- generateFormPost $ editUserForm (Just updated_user)

                    defaultLayout $
                        previewWidget form "update" $
                            renderUser (Just viewer_id) user_id updated_user mempty
        _ -> do
            alertDanger "Failed to update user."
            redirect (UserR user_id)
