module Handler.User where

import Import

import Model.User
import Model.Role

import Widgets.Sidebar
import Widgets.Markdown
import Widgets.ProjectPledges

import Yesod.Markdown

import Yesod.Auth.HashDB (setPassword)

import Control.Exception.Lifted (throwIO, handle)

hiddenMarkdown :: RenderMessage master FormMessage => Maybe Markdown -> AForm sub master (Maybe Markdown)
hiddenMarkdown Nothing = fmap (fmap Markdown) $ aopt hiddenField "" Nothing
hiddenMarkdown (Just (Markdown str)) = fmap (fmap Markdown) $ aopt hiddenField "" $ Just $ Just str

editUserForm :: User -> Form UserUpdate
editUserForm user = renderDivs $
    UserUpdate
        <$> aopt textField "Public Name" (Just $ userName user)
        <*> aopt textField "Avatar (link)" (Just $ userAvatar user)
        <*> aopt snowdriftMarkdownField "Blurb (used on listings of many people)" (Just $ userBlurb user)
        <*> aopt snowdriftMarkdownField "Personal Statement (visible only on this page)" (Just $ userStatement user)

previewUserForm :: User -> Form UserUpdate
previewUserForm user = renderDivs $
    UserUpdate
        <$> aopt hiddenField "" (Just $ userName user)
        <*> aopt hiddenField "" (Just $ userAvatar user)
        <*> hiddenMarkdown (userBlurb user)
        <*> hiddenMarkdown (userStatement user)

getUserR :: UserId -> Handler RepHtml
getUserR user_id = do
    viewer_id <- requireAuthId
    user <- runDB $ get404 user_id

    defaultLayout $ renderUser viewer_id user_id user


renderUser :: UserId -> UserId -> User -> Widget
renderUser viewer_id user_id user = do
    let is_owner = user_id == viewer_id
        user_entity = Entity user_id user

    $(widgetFile "user")
            


getEditUserR :: UserId -> Handler RepHtml
getEditUserR user_id = do
    viewer_id <- requireAuthId
    when (user_id /= viewer_id) $ permissionDenied "You can only modify your own profile!"

    user <- runDB $ get404 user_id

    (form, enctype) <- generateFormPost $ editUserForm user
    defaultLayout $(widgetFile "edit_user")


postUserR :: UserId -> Handler RepHtml
postUserR user_id = do
    viewer_id <- requireAuthId
    if user_id /= viewer_id
     then permissionDenied "You can only modify your own profile!"
     else do
        ((result, _), _) <- runFormPost $ editUserForm undefined

        case result of
            FormSuccess user_update -> do
                mode <- lookupPostParam "mode"
                let action :: Text = "update"
                case mode of
                    Just "preview" -> do
                        user <- runDB $ get404 user_id
                        let updated_user = applyUserUpdate user user_update
                            rendered_user = renderUser viewer_id user_id updated_user

                        (hidden_form, _) <- generateFormPost $ previewUserForm updated_user

                        let preview_controls = [whamlet|
                            <div .row>
                                <div .span9>
                                    <form method="POST" action="@{UserR user_id}">
                                        ^{hidden_form}
                                        <div .alert>
                                            This is a preview; your changes have not been saved!
                                        <script>
                                            document.write('<input type="submit" value="edit" onclick="history.go(-1);return false;" />')
                                        <input type=submit name=mode value=#{action}>
                        |]

                        defaultLayout [whamlet|
                            ^{preview_controls}
                            ^{rendered_user}
                            ^{preview_controls}
                        |]

                    Just x | x == action -> do
                        runDB $ updateUser user_id user_update
                        redirect $ UserR user_id

                    _ -> do
                        setMessage "Error: unknown mode."
                        redirect $ UserR user_id

            _ -> do
                setMessage "Failed to update user."
                redirect $ UserR user_id

getUsersR :: Handler RepHtml
getUsersR = do
    Entity _ viewer <- requireAuth

    when (userRole viewer /= Admin) $ permissionDenied "Only admins can view all users."

    users <- runDB $ selectList [] [ Asc UserId ]

    defaultLayout $(widgetFile "users")


getUserCreateR :: Handler RepHtml
getUserCreateR = do
    (form, _) <- generateFormPost $ userCreateForm Nothing
    defaultLayout $ [whamlet|
        <form method=POST>
            ^{form}
            <input type=submit>
    |]


postUserCreateR :: Handler RepHtml
postUserCreateR = do
    ((result, form), _) <- runFormPost $ userCreateForm Nothing

    case result of
        FormSuccess (ident, passwd, name, avatar) -> do
            now <- liftIO getCurrentTime
            success <- handle (\ DBException -> return False) $ runDB $ do
                account_id <- insert $ Account 0
                user <- setPassword passwd $ User ident Nothing Nothing name account_id Uninvited avatar Nothing Nothing now now now now
                uid_maybe <- insertUnique user
                lift $ case uid_maybe of
                    Just uid -> do
                        setMessage $ toHtml $ "created user; welcome! (" ++ show account_id ++ ", " ++ show uid ++ ")"
                        return True

                    Nothing -> do
                        setMessage "E-mail or handle already in use."
                        throwIO DBException

            when success $ do
                setCreds True $ Creds "HashDB" ident []
                redirectUltDest HomeR

        FormMissing -> setMessage "missing field"
        FormFailure strings -> setMessage (toHtml $ mconcat strings)

    defaultLayout $ [whamlet|
        <form method=POST>
            ^{form}
            <input type=submit>
    |]
    

userCreateForm :: Maybe Text -> Form (Text, Text, Maybe Text, Maybe Text)
userCreateForm ident extra = do
    (identRes, identView) <- mreq textField "" ident
    (passwd1Res, passwd1View) <- mreq passwordField "" Nothing
    (passwd2Res, passwd2View) <- mreq passwordField "" Nothing
    (nameRes, nameView) <- mopt textField "" Nothing
    (avatarRes, avatarView) <- mopt textField "" Nothing

    let view = [whamlet|
        ^{extra}
        <table .table>
            <tr>
                <td>
                    E-mail or handle (private):
                <td>
                    ^{fvInput identView}
            <tr>
                <td>
                    Password:
                <td>
                    ^{fvInput passwd1View}
            <tr>
                <td>
                    Repeat Password:
                <td>
                    ^{fvInput passwd2View}
            <tr>
                <td>
                    Name (public, optional):
                <td>
                    ^{fvInput nameView}
            <tr>
                <td>
                    Avatar (link, optional):
                <td>
                    ^{fvInput avatarView}
    |]

        passwdRes = case (passwd1Res, passwd2Res) of
            (FormSuccess a, FormSuccess b) -> if a == b then FormSuccess a else FormFailure ["passwords do not match"]
            (FormSuccess _, x) -> x
            (x, _) -> x

        result = (,,,) <$> identRes <*> passwdRes <*> nameRes <*> avatarRes

    return (result, view)
