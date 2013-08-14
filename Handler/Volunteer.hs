module Handler.Volunteer where

import Import

import Widgets.Sidebar

import Control.Arrow ((&&&))


volunteerForm :: UTCTime -> [Entity Interest] -> Entity User -> Form (VolunteerApplication, [InterestId])
volunteerForm now interests (Entity user_id user) = renderBootstrap $
    (\ name email contact website location experience interest_ids referral other ->
        (VolunteerApplication now user_id name email contact website location experience referral other, interest_ids)
    )   <$> areq textField "Name or Internet Handle:" Nothing
        <*> areq emailField "E-mail:" (Just . userIdent $ user)
        <*> aopt textField "Other contact info (phone, IRC handle, chat ID, etc):" Nothing
        <*> aopt textField "Website URL:" Nothing
        <*> aopt textField "Location:" Nothing
        <*> aopt textareaField "Relevant work/training/volunteer experience:" Nothing
        <*> areq (multiSelectFieldList $ (interestDescription . entityVal &&& entityKey) <$> interests) "Areas of interest (use ctrl to select multiple):" Nothing
        <*> aopt (selectFieldList source) "How did you hear about Snowdrift.coop?" Nothing
        <*> aopt textareaField "Anything else you'd like us to know:" Nothing 
   where
        source = (\ a -> zip a a) $ ["Article", "Weblink", "Conference", "Search engine", "Personal recommendation"]


getVolunteerR :: Handler RepHtml
getVolunteerR = do
    user <- requireAuth
    now <- liftIO getCurrentTime
    interests <- runDB $ selectList [] []
    (volunteer_form, _) <- generateFormPost $ volunteerForm now interests user
    defaultLayout $(widgetFile "volunteer")


postVolunteerR :: Handler RepHtml
postVolunteerR = do
    user <- requireAuth
    now <- liftIO getCurrentTime
    interests <- runDB $ selectList [] []
    ((result, _), _) <- runFormPost $ volunteerForm now interests user

    case result of
        FormSuccess (application, interest_ids) -> do
            runDB $ do
                application_id <- insert application
                forM_ interest_ids $ \ interest_id -> insert $ VolunteerInterest application_id interest_id
            
            setMessage "application submitted"
            redirect VolunteerR

        _ -> do
            setMessage "error submitting application"
            redirect VolunteerR

