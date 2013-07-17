module Handler.Committee where

import Import

import Widgets.Sidebar


committeeForm :: UTCTime -> Entity User -> Form CommitteeApplication
committeeForm now (Entity user_id user) = renderBootstrap $
    CommitteeApplication now user_id
        <$> areq textField "Full name:" Nothing
        <*> areq emailField "E-mail:" (Just . userIdent $ user)
        <*> aopt textField "Other contact info (website URL, phone, chat ID, etc):" Nothing
        <*> areq textField "Occupation(s):" Nothing
        <*> areq textField "Location:" Nothing
        <*> aopt textareaField "Relevant expertise:" Nothing
        <*> areq textareaField "Personal statement (why you want to join the committee):" Nothing
        <*> aopt textareaField "Any other comments:" Nothing

getCommitteeR :: Handler Html
getCommitteeR = do
    user <- requireAuth
    now <- liftIO getCurrentTime
    (committee_form, _) <- generateFormPost $ committeeForm now user
    defaultLayout $(widgetFile "committee")


postCommitteeR :: Handler Html
postCommitteeR = do
    user <- requireAuth
    now <- liftIO getCurrentTime
    ((result, _), _) <- runFormPost $ committeeForm now user

    case result of
        FormSuccess application -> do
            _ <- runDB $ insert application
            setMessage "application submitted"
            redirect CommitteeR

        _ -> do
            setMessage "error submitting application"
            redirect CommitteeR

