module Handler.ResetPassphrase (getResetPassphraseR, postResetPassphraseR) where

import Import hiding (isNothing)

import Data.Maybe (fromJust, isNothing)

import Dev
import Handler.TH
import Handler.Utils
import Model.ResetPassphrase
import View.ResetPassphrase (resetPassphraseForm)
import qualified View.ResetPassphrase as View

getResetPassphraseR :: Handler Html
getResetPassphraseR = do
    (form, enctype) <- generateFormPost resetPassphraseForm
    $(widget "reset-passphrase" "Reset Passphrase")

initResetPassphrase :: UserId -> Text -> Handler Html
initResetPassphrase user_id email = do
    hash <- liftIO newHash
    uri  <- getUrlRender <*> pure (UserResetPassphraseR user_id hash)
    runDB $ insert_ $ ResetPassphrase user_id email uri False
    alertSuccess "Sent an email with further instructions."
    redirect HomeR

postResetPassphraseR :: Handler Html
postResetPassphraseR = do
    ((result, form), enctype) <- runFormPost resetPassphraseForm
    let alertAndRefresh msg = do
            alertDanger msg
            $(widget "reset-passphrase" "Reset Passphrase")
    case result of
        FormSuccess View.ResetPassphrase {..} ->
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
                              in initResetPassphrase user_id email
                         else alertAndRefresh $ "Sorry, either your handle is invalid "
                                             <> "or we have no verified email for you."
               | isJust rpEmail -> do
                     let email = fromJust rpEmail
                     muser_id <-
                         runDB $ selectUserIdIfVerified email
                     if isJust muser_id
                         then let user_id = fromJust muser_id
                              in initResetPassphrase user_id email
                         else alertAndRefresh $ "Either the email is invalid "
                                             <> "or not verified."
        _ -> alertAndRefresh "Failed to reset the passphrase."
