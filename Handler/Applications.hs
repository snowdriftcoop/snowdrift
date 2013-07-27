module Handler.Applications where

import Import

import Model.Role

import Widgets.Sidebar

getApplicationsR :: Handler RepHtml
getApplicationsR = do
    Entity viewer_id viewer <- requireAuth
    now <- liftIO getCurrentTime

    applications <- 
        if userRole viewer == CommitteeMember || userRole viewer == Admin
         then runDB $ selectList [] [ Desc VolunteerApplicationCreatedTs ]
         else return []

    _ <- runDB $ update viewer_id [ UserReadApplications =. now ]


    defaultLayout $(widgetFile "applications")
