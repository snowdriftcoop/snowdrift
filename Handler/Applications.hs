module Handler.Applications where

import Import

-- import Model.Role

import Widgets.Sidebar

getApplicationsR :: Handler Html
getApplicationsR = do
    Entity viewer_id viewer <- requireAuth
    now <- liftIO getCurrentTime

    let applications = []
    {-
    applications <- 
        if userRole viewer == CommitteeMember || userRole viewer == Admin
         then runDB $ selectList [] [ Desc CommitteeApplicationCreatedTs ]
         else return []
    -}

    _ <- runDB $ update $ \ user -> do
            set user [ UserReadApplications =. val now ]
            where_ (user ^. UserId ==. val viewer_id)


    defaultLayout $(widgetFile "applications")
