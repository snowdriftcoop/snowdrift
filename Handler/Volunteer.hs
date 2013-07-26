module Handler.Volunteer where

import Import

import Widgets.Sidebar


volunteerForm :: UTCTime -> Entity User -> Form VolunteerApplication
volunteerForm now (Entity user_id user) = renderBootstrap $
    VolunteerApplication now user_id
        <$> areq textField "Name or Internet Handle:" Nothing
        <*> areq emailField "E-mail:" (Just . userIdent $ user)
        <*> aopt textField "Other contact info (phone, IRC handle, chat ID, etc):" Nothing
        <*> aopt textField "Website URL:" Nothing
        <*> aopt textField "Location:" Nothing
        <*> aopt textareaField "Relevant work/training/volunteer experience:" Nothing
        <*> areq (selectFieldList interest) "Area of interest:" Nothing
        <*> aopt textareaField "Anything else you'd like us to know:" Nothing 
      where
        interest = (\ a -> zip a a) $ ["Programming/Debugging", "FLO Culture/Journalism", "Web Development", "Cooperative Development", "Legal Matters", "Crowdfunding/Microfinance", "Illustration/Graphic Design", "Education/Research", "Marketing/Recruitment"] 

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

