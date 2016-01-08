module Handler.ResetPassphrase (getResetPasswordR, postResetPasswordR) where

import Import hiding (isNothing)

import Data.Maybe (fromJust, isNothing)

import Dev
import Handler.TH
import Handler.Utils
import Model.ResetPassphrase
import View.ResetPassphrase (resetPasswordForm)
import qualified View.ResetPassphrase as View

getResetPasswordR :: Handler Html
getResetPasswordR = do
    (form, enctype) <- generateFormPost resetPasswordForm
    $(widget "reset-password" "Reset Password")

initResetPassword :: UserId -> Text -> Handler Html
initResetPassword user_id email = do
    hash <- liftIO newHash
    uri  <- getUrlRender <*> pure (UserResetPasswordR user_id hash)
    runDB $ insert_ $ ResetPassword user_id email uri False
    alertSuccess "Sent an email with further instructions."
    redirect HomeR

postResetPasswordR :: Handler Html
postResetPasswordR = do
    ((result, form), enctype) <- runFormPost resetPasswordForm
    let alertAndRefresh msg = do
            alertDanger msg
            $(widget "reset-password" "Reset Password")
    case result of
        FormSuccess View.ResetPassword {..} ->
            if | isNothing rpHandle && isNothing rpEmail ->
                     alertAndRefresh $ "Neither the handle nor the email "
                                    <> "is specified."
               | isJust rpHandle && isJust rpEmail ->
                     alertAndRefresh "Specify either the handle or the email."
               | isJust rpHandle -> do
                     let handle = fromJust rpHandle
                     muser_id_and_email <-
                         runDB $ selectUserIdAndEmailIfVerified handle
                     if isJust muser_id_and_email
                         then let (user_id, email) = fromJust muser_id_and_email
                              in initResetPassword user_id email
                         else alertAndRefresh $ "Sorry, either your handle is invalid "
                                             <> "or we have no verified email for you."
               | isJust rpEmail -> do
                     let email = fromJust rpEmail
                     muser_id <-
                         runDB $ selectUserIdIfVerified email
                     if isJust muser_id
                         then let user_id = fromJust muser_id
                              in initResetPassword user_id email
                         else alertAndRefresh $ "Either the email is invalid "
                                             <> "or not verified."
        _ -> alertAndRefresh "Failed to reset the password."
