module Handler.Applications where

import Import

-- import Model.Role

import Widgets.Sidebar

import qualified Data.Map as M

import Control.Arrow

getApplicationsR :: Handler Html
getApplicationsR = do
    Entity viewer_id viewer <- requireAuth
    now <- liftIO getCurrentTime

    let applications_map = M.fromListWith (++) $ second return <$> [] 
    {-
    applications <- 
        if userRole viewer == CommitteeMember || userRole viewer == Admin
         then runDB $ selectList [] [ Desc VolunteerApplicationCreatedTs ]
         else return []
    -}

    _ <- runDB $ update $ \ user -> do
            set user [ UserReadApplications =. val now ]
            where_ (user ^. UserId ==. val viewer_id)


    defaultLayout $(widgetFile "applications")
