module Handler.Application where

import Import

import Model.User

import Widgets.Sidebar

import qualified Data.Text as T

getApplicationR :: Text -> VolunteerApplicationId -> Handler Html
getApplicationR project_handle application_id = do
    Entity viewer_id viewer <- requireAuth

    affiliated <- runDB $ (||)
        <$> isProjectAffiliated project_handle viewer_id
        <*> isProjectAdmin "snowdrift" viewer_id

    when (not affiliated) $ permissionDenied "you must be affiliated with this project to view applications"

    (application, user) <- runDB $ do
        application <- get404 application_id
        let user_id = volunteerApplicationUser application
        user <- get404 user_id
        return (application, Entity user_id user)

    interests :: [Value Text] <- runDB $ select $ from $ \ (volunteer_interest `InnerJoin` interest) -> do
        on_ $ interest ^. InterestId ==. volunteer_interest ^. VolunteerInterestInterest
        where_ $ volunteer_interest ^. VolunteerInterestVolunteer ==. val application_id
        return (interest ^. InterestDescription)

    let rendered_interests = T.intercalate ", " $ map (\ (Value x) -> x) interests

    defaultLayout $(widgetFile "application")
