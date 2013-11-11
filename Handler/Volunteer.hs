module Handler.Volunteer where

import Import

import Widgets.Sidebar


volunteerForm :: UTCTime -> ProjectId -> [Entity Interest] -> Entity User -> Form (VolunteerApplication, [InterestId])
volunteerForm now project_id interests (Entity user_id user) = renderBootstrap $
    (\ name email contact website location experience interest_ids other ->
        (VolunteerApplication now project_id user_id name email contact website location experience other, interest_ids)
    )   <$> areq textField "Name or Internet Handle:" Nothing
        <*> areq emailField "E-mail:" (Just . userIdent $ user)
        <*> aopt textField "Other contact info (phone, IRC handle, chat ID, etc):" Nothing
        <*> aopt textField "Website URL:" Nothing
        <*> aopt textField "Location:" Nothing
        <*> aopt textareaField "Relevant work/training/volunteer experience:" Nothing
        <*> areq (multiSelectFieldList $ (interestDescription . entityVal &&& entityKey) <$> interests) "Areas of interest (use ctrl to select multiple):" Nothing
        <*> aopt textareaField "Anything else you'd like us to know:" Nothing 
   where
        source = (\ a -> zip a a) $ ["Article", "Weblink", "Conference", "Search engine", "Personal recommendation"]


getVolunteerR :: Text -> Handler Html
getVolunteerR project_handle = do
    user <- requireAuth
    now <- liftIO getCurrentTime
    Entity project_id _ <- runDB $ getBy404 $ UniqueProjectHandle project_handle
    interests <- runDB $ select $ from $ return
    (volunteer_form, _) <- generateFormPost $ volunteerForm now project_id interests user
    defaultLayout $(widgetFile "volunteer")


postVolunteerR :: Text -> Handler Html
postVolunteerR project_handle = do
    user <- requireAuth
    now <- liftIO getCurrentTime
    Entity project_id _ <- runDB $ getBy404 $ UniqueProjectHandle project_handle
    interests <- runDB $ select $ from $ return
    ((result, _), _) <- runFormPost $ volunteerForm now project_id interests user

    case result of
        FormSuccess (application, interest_ids) -> do
            runDB $ do
                application_id <- insert application
                forM_ interest_ids $ \ interest_id -> insert $ VolunteerInterest application_id interest_id

            setMessage "application submitted"
            redirect $ VolunteerR project_handle

        _ -> do
            setMessage "error submitting application"
            redirect $ VolunteerR project_handle

