module Handler.User where

import Import

import Model.User
import Model.Role

import Widgets.Sidebar
import Widgets.ProjectPledges

import Yesod.Markdown

hiddenMarkdown :: RenderMessage master FormMessage => Maybe Markdown -> AForm sub master (Maybe Markdown)
hiddenMarkdown Nothing = fmap (fmap Markdown) $ aopt hiddenField "" Nothing
hiddenMarkdown (Just (Markdown str)) = fmap (fmap Markdown) $ aopt hiddenField "" $ Just $ Just str

editUserForm :: User -> Form UserUpdate
editUserForm user = renderDivs $
    UserUpdate
        <$> aopt textField "Name" (Just $ userName user)
        <*> aopt textField "Avatar (link)" (Just $ userAvatar user)
        <*> aopt markdownField "Blurb" (Just $ userBlurb user)
        <*> aopt markdownField "Personal Statement" (Just $ userStatement user)

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
                                        <em>
                                            This is a preview; your changes have not been saved!
                                        <br>
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

