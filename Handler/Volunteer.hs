module Handler.Volunteer where

import Import

import Widgets.Sidebar


volunteerForm :: UTCTime -> Entity User -> Form VolunteerApplication
volunteerForm now (Entity user_id user) = renderBootstrap $
    VolunteerApplication now user_id
        <$> areq textField "Full name:" Nothing
        <*> areq emailField "E-mail:" (Just . userIdent $ user)
        <*> aopt textField "Other contact info (website URL, phone, chat ID, etc):" Nothing
        <*> areq textField "Occupation(s):" Nothing
        <*> areq textField "Location:" Nothing
        <*> aopt textareaField "Relevant expertise:" Nothing
        <*> areq textareaField "Personal statement (why you want to join the committee):" Nothing
        <*> aopt textareaField "Any other comments:" Nothing

getVolunteerR :: Handler RepHtml
getVolunteerR = do
    user <- requireAuth
    now <- liftIO getCurrentTime
    (volunteer_form, _) <- generateFormPost $ volunteerForm now user
    defaultLayout $(widgetFile "volunteer")


postVolunteerR :: Handler RepHtml
postVolunteerR = do
    user <- requireAuth
    now <- liftIO getCurrentTime
    ((result, _), _) <- runFormPost $ volunteerForm now user

    case result of
        FormSuccess application -> do
            _ <- runDB $ insert application
            setMessage "application submitted"
            redirect VolunteerR

        _ -> do
            setMessage "error submitting application"
            redirect VolunteerR

