module Handler.Application where

import Import

import Model.User

import Widgets.Sidebar

getApplicationR :: CommitteeApplicationId -> Handler Html
getApplicationR application_id = do
    (application, user) <- runDB $ do
        application <- get404 application_id
        let user_id = committeeApplicationUser application
        user <- get404 user_id
        return (application, Entity user_id user)

    defaultLayout $(widgetFile "application")
