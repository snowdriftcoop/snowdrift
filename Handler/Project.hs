module Handler.Project where

import Import hiding ((=.), (==.), update)

import Model.Currency
import Model.Project
import Model.Shares
import Model.Markdown.Diff
import Model.Role
import Model.User

import qualified Data.Text as T

import Widgets.Sidebar

import Database.Esqueleto

lookupGetParamDefault :: Read a => Text -> a -> Handler a
lookupGetParamDefault name def = do
    maybe_value <- lookupGetParam name
    return $ fromMaybe def $ maybe_value >>= readMaybe . T.unpack


getProjectsR :: Handler RepHtml
getProjectsR = do
    page <- lookupGetParamDefault "page" 0
    per_page <- lookupGetParamDefault "count" 20
    projects <- runDB $ selectList [] [ Asc ProjectCreatedTs, LimitTo per_page, OffsetBy page ]
    defaultLayout $(widgetFile "projects")


getProjectR :: ProjectId -> Handler RepHtml
getProjectR project_id = do
    maybe_viewer_id <- maybeAuthId

    (project, pledges, pledge) <- runDB $ do
        project <- get404 project_id
        pledges <- getProjectShares project_id
        pledge <- case maybe_viewer_id of
            Nothing -> return Nothing
            Just viewer_id -> getBy $ UniquePledge viewer_id project_id

        return (project, pledges, pledge)

    let share_value = projectShareValue project
        users = fromIntegral $ length pledges
        shares = sum pledges
        project_value = share_value $* (fromIntegral shares)
        description = markdownToHtml $ projectDescription project

    ((_, update_shares), _) <- generateFormGet $ buySharesForm $ fromMaybe 0 $ pledgeShares . entityVal <$> pledge

    defaultLayout $(widgetFile "project")


guardCanEdit :: ProjectId -> Entity User -> Handler ()
guardCanEdit project_id (Entity user_id user) =
    when (userRole user /= Admin) $ do
        match <- runDB $ select $ from $ \( project_user ) -> do
            where_ ( project_user ^. ProjectUserUser ==. val user_id &&. project_user ^. ProjectUserProject ==. val project_id &&. project_user ^. ProjectUserCanEdit ==. val True )
            limit 1
            return project_user

        when (null match) $
            permissionDenied "You do not have permission to edit this project."


data UpdateProject = UpdateProject { updateProjectName :: Text, updateDescription :: Markdown }


editProjectForm :: Maybe Project -> Form UpdateProject
editProjectForm project =
    renderDivs $ UpdateProject
        <$> areq textField "Project Name" (projectName <$> project)
        <*> areq markdownField "Description" (projectDescription <$> project)


getEditProjectR :: ProjectId -> Handler RepHtml
getEditProjectR project_id = do
    requireAuth >>= guardCanEdit project_id

    project <- runDB $ get project_id

    (project_form, _) <- generateFormPost $ editProjectForm project

    defaultLayout $(widgetFile "edit_project")


postProjectR :: ProjectId -> Handler RepHtml
postProjectR project_id = do
    viewer <- requireAuth
    guardCanEdit project_id viewer
    ((result, _), _) <- runFormPost $ editProjectForm Nothing

    now <- liftIO getCurrentTime

    case result of
        FormSuccess (UpdateProject name description) -> do
            processed <- runDB $ do
                maybe_project <- get project_id
                case maybe_project of
                    Nothing -> return False
                    Just project -> do
                        when (projectDescription project /= description) $ do
                            project_update <- insert $ ProjectUpdate now project_id (entityKey viewer) $ diffMarkdown (projectDescription project) description
                            last_update <- getBy $ UniqueProjectLastUpdate project_id
                            case last_update of
                                Just (Entity key _) -> repsert key $ ProjectLastUpdate project_id project_update
                                Nothing -> (insert $ ProjectLastUpdate project_id project_update) >> return ()

                        update $ \ p -> do
                            set p [ ProjectName =. val name, ProjectDescription =. val description ]
                            where_ (p ^. ProjectId ==. val project_id)

                        return True

            if processed
             then setMessage "project updated"
             else notFound

        _ -> setMessage "error"

    redirect $ ProjectR project_id


getProjectDonorsR :: ProjectId -> Handler RepHtml
getProjectDonorsR project_id = do
    page <- lookupGetParamDefault "page" 0
    per_page <- lookupGetParamDefault "count" 20

    (project, pledges) <- runDB $ do
        project <- get404 project_id
        pledges <- select $ from $ \(pledge `InnerJoin` user) -> do
            on (pledge ^. PledgeUser ==. user ^. UserId)
            where_ (pledge ^. PledgeProject ==. val project_id)
            orderBy [ desc (pledge ^. PledgeShares), asc (user ^. UserName), asc (user ^. UserId)]
            offset page
            limit per_page
            return (pledge, user)

        return (project, pledges)

    defaultLayout $(widgetFile "project_donors")
