-- Applications.hs (plural) is for viewing the *list* of all volunteer applications for a project

module Handler.Applications where

import Import

import Model.User

getApplicationsR :: Text -> Handler Html
getApplicationsR project_handle = do
    viewer_id <- requireAuthId
    now <- liftIO getCurrentTime

    -- let applications_map = M.fromListWith (++) $ map (id &&& return) $ applications

    affiliated <- runDB $ (||)
        <$> isProjectAffiliated project_handle viewer_id
        <*> isProjectAdmin "snowdrift" viewer_id

    unless affiliated $
        permissionDenied "you must be affiliated with this project to view applications"

    Entity _ project <- runYDB $ getBy404 $ UniqueProjectHandle project_handle
    applications <- runYDB $ do
        project_id <- fmap entityKey $ getBy404 $ UniqueProjectHandle project_handle
        select $
         from $ \ application -> do
         where_ $ application ^. VolunteerApplicationProject ==. val project_id
         orderBy [ desc $ application ^. VolunteerApplicationCreatedTs ]
         return application

    runDB $
        update $ \ user -> do
        set user [ UserReadApplications =. val now ]
        where_ (user ^. UserId ==. val viewer_id)

    defaultLayout $ do
        setTitle . toHtml $
            projectName project <> " Volunteer Applications | Snowdrift.coop"
        $(widgetFile "applications")
