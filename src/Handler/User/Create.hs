module Handler.User.Create where

import Import

import Data.Maybe (fromJust)

import Handler.Utils
import View.User

getUserCreateR :: Handler Html
getUserCreateR = do
    (form, _) <- generateFormPost $ createUserForm Nothing
    defaultLayout $ do
        snowdriftTitle "Create User"
        [whamlet|
            <form method=POST>
                ^{form}
                <input type=submit>
        |]

-- | This needs to be placed in a folder/module for emails.
startEmailVerification :: UserId -> Text -> HandlerT App IO ()
startEmailVerification user_id user_email = do
    hash    <- liftIO newHash
    ver_uri <- getUrlRender <*> (pure $ UserVerifyEmailR user_id hash)
    runDB $ do
        insert_ $ EmailVerification user_id user_email ver_uri False
        update $ \u -> do
            set u $ [UserEmail_verified =. val False]
            where_ $ u ^. UserId ==. val user_id
    alertSuccess $ "Verification email has been sent to " <> user_email <> "."

postUserCreateR :: Handler Html
postUserCreateR = do
    ((result, form), _) <- runFormPost $ createUserForm Nothing

    case result of
        FormSuccess (ident, passph, name, memail, avatar, nick) -> do
            createUser ident (Just passph) name (NewEmail False <$> memail) avatar nick
                >>= \muser_id -> when (isJust muser_id) $ do
                    when (isJust memail) $ do
                        let email   = fromJust memail
                            user_id = fromJust muser_id
                        startEmailVerification user_id email
                    setCreds True $ Creds "hashdb" ident []
                    redirectUltDest HomeR

        FormMissing -> alertDanger "missing field"
        FormFailure strings -> alertDanger (mconcat strings)

    defaultLayout $ [whamlet|
        <form method=POST>
            ^{form}
            <input type=submit>
    |]
