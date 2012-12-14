module Handler.User where

import Import

import Model.User

import Widgets.Sidebar
import Widgets.ProjectPledges

userForm :: User -> Form UserUpdate
userForm user = renderDivs $
    UserUpdate
        <$> aopt textField "Name" (Just $ userName user)
        <*> aopt textField "Avatar (link)" (Just $ userAvatar user)
        <*> aopt markdownField "Blurb" (Just $ userBlurb user)
        <*> aopt markdownField "Personal Statement" (Just $ userStatement user)

getUserR :: UserId -> Handler RepHtml
getUserR user_id = do
    viewer_id <- requireAuthId
    user <- runDB $ get404 user_id

    let is_owner = user_id == viewer_id
        user_entity = Entity user_id user

    defaultLayout $(widgetFile "user")
            


getEditUserR :: UserId -> Handler RepHtml
getEditUserR user_id = do
    viewer_id <- requireAuthId
    when (user_id /= viewer_id) $ permissionDenied "You can only modify your own profile!"

    user <- runDB $ get404 user_id

    (form, enctype) <- generateFormPost $ userForm user
    defaultLayout $(widgetFile "user_form")


postUserR :: UserId -> Handler RepHtml
postUserR user_id = do
    viewer_id <- requireAuthId
    if user_id /= viewer_id
     then permissionDenied "You can only modify your own profile!"
     else do
        ((result, _), _) <- runFormPost $ userForm undefined
        case result of
            FormSuccess user_update -> do
                runDB $ updateUser user_id user_update
                redirect $ UserR user_id

            _ -> do
                setMessage "Failed to update user."
                redirect $ UserR user_id
