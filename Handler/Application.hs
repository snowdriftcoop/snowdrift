module Handler.Application where

import Import hiding ((==.), Value)

import Model.User

import Widgets.Sidebar

import Database.Esqueleto

import qualified Data.Text as T

getApplicationR :: VolunteerApplicationId -> Handler RepHtml
getApplicationR application_id = do
    (application, user) <- runDB $ do
        application <- get404 application_id
        let user_id = volunteerApplicationUser application
        user <- get404 user_id
        return (application, Entity user_id user)

    interests :: [Value Text] <- runDB $ select $ from $ \ (volunteer_interest `InnerJoin` interest) -> do
        on (interest ^. InterestId ==. volunteer_interest ^. VolunteerInterestInterest)
        where_ (volunteer_interest ^. VolunteerInterestVolunteer ==. val application_id)
        return (interest ^. InterestDescription)

    let rendered_interests = T.intercalate ", " $ map (\ (Value x) -> x) interests

    defaultLayout $(widgetFile "application")
