{-# LANGUAGE TupleSections #-}

module Handler.Project where

import Import hiding ((=.), (==.), update, delete)

import Model.Currency
import Model.Project
import Model.Shares
import Model.Markdown.Diff
import Model.Role
import Model.User

import qualified Data.Text as T
import qualified Data.Set as S

import Widgets.Sidebar

import Database.Esqueleto

import Control.Monad (forM, forM_)

import Yesod.Markdown

lookupGetParamDefault :: Read a => Text -> a -> Handler a
lookupGetParamDefault name def = do
    maybe_value <- lookupGetParam name
    return $ fromMaybe def $ maybe_value >>= readMaybe . T.unpack


getProjectsR :: Handler RepHtml
getProjectsR = do
    page <- lookupGetParamDefault "page" 0
    per_page <- lookupGetParamDefault "count" 20
    tags <- maybe [] (map T.strip . T.splitOn ",") <$> lookupGetParam "tags"
    projects <- runDB $ if null tags
        then selectList [] [ Asc ProjectCreatedTs, LimitTo per_page, OffsetBy page ]
        else do
            tagged_projects <- forM tags $ \ name -> select $ from $ \ (t `InnerJoin` p_t) -> do
                on (t ^. TagId ==. p_t ^. ProjectTagTag)
                where_ ( t ^. TagName ==. val name )
                return p_t

            let project_ids = if null tagged_projects then S.empty else foldl1 S.intersection $ map (S.fromList . map (projectTagProject . entityVal)) tagged_projects
            selectList [ ProjectId <-. S.toList project_ids ] [ Asc ProjectCreatedTs, LimitTo per_page, OffsetBy page ]

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

    defaultLayout $ renderProject (Just project_id) project pledges pledge


renderProject :: Maybe ProjectId -> ProjectGeneric SqlPersist -> [Int64] -> Maybe (Entity (PledgeGeneric SqlPersist)) -> Widget
renderProject maybe_project_id project pledges pledge = do
    let share_value = projectShareValue project
        users = fromIntegral $ length pledges
        shares = sum pledges
        project_value = share_value $* (fromIntegral shares)
        description = markdownToHtml $ projectDescription project

    ((_, update_shares), _) <- lift $ generateFormGet $ buySharesForm $ fromMaybe 0 $ pledgeShares . entityVal <$> pledge

    $(widgetFile "project")


guardCanEdit :: ProjectId -> Entity User -> Handler ()
guardCanEdit project_id (Entity user_id user) =
    when (userRole user /= Admin) $ do
        match <- runDB $ select $ from $ \( project_user ) -> do
            where_ ( project_user ^. ProjectUserUser ==. val user_id &&. project_user ^. ProjectUserProject ==. val project_id &&. project_user ^. ProjectUserCanEdit ==. val True )
            limit 1
            return project_user

        when (null match) $
            permissionDenied "You do not have permission to edit this project."


data UpdateProject = UpdateProject { updateProjectName :: Text, updateProjectDescription :: Markdown, updateProjectTags :: [Text] }


editProjectForm :: Maybe (Project, [Text]) -> Form UpdateProject
editProjectForm project =
    renderDivs $ UpdateProject
        <$> areq textField "Project Name" (projectName . fst <$> project)
        <*> areq markdownField "Description" (projectDescription . fst <$> project)
        <*> (map T.strip . T.splitOn "," <$> areq textField "Tags" (T.intercalate ", " . snd <$> project))

previewProjectForm :: Maybe (Project, [Text]) -> Form UpdateProject
previewProjectForm project =
    renderDivs $ UpdateProject
        <$> areq hiddenField "" (projectName . fst <$> project)
        <*> (Markdown <$> areq hiddenField "" ((\ (Markdown str) -> str) . projectDescription . fst <$> project))
        <*> (map T.strip . T.splitOn "," <$> areq hiddenField "" (T.intercalate ", " . snd <$> project))


getEditProjectR :: ProjectId -> Handler RepHtml
getEditProjectR project_id = do
    requireAuth >>= guardCanEdit project_id

    project <- runDB $ get project_id
    tags <- runDB $ select $ from $ \ (p_t `InnerJoin` tag) -> do
        on (p_t ^. ProjectTagTag ==. tag ^. TagId)
        where_ (p_t ^. ProjectTagProject ==. val project_id)
        return tag

    (project_form, _) <- generateFormPost $ editProjectForm ((, map (tagName . entityVal) tags) <$> project)

    defaultLayout $(widgetFile "edit_project")


postProjectR :: ProjectId -> Handler RepHtml
postProjectR project_id = do
    viewer <- requireAuth
    guardCanEdit project_id viewer
    ((result, _), _) <- runFormPost $ editProjectForm Nothing

    now <- liftIO getCurrentTime

    case result of
        FormSuccess (UpdateProject name description tags) -> do
            mode <- lookupPostParam "mode"
            case mode of
                Just "preview" -> do
                    project <- runDB $ get404 project_id
                    let preview_project = project { projectName = name, projectDescription = description }
                    (hidden_form, _) <- generateFormPost $ previewProjectForm $ Just (preview_project, tags)
                    let rendered_project = renderProject (Just project_id) preview_project [] Nothing
                    defaultLayout $ [whamlet|
                        <div .row>
                            <div .span9>
                                <form method="POST" action="@{ProjectR project_id}">
                                    ^{hidden_form}
                                    <em>
                                        This is a preview.
                                    <br>
                                    <script>
                                        document.write('<input type="submit" value="edit" onclick="history.go(-1);return false;" />')
                                    <input type=submit name=mode value=update>
                        ^{rendered_project}
                        <div .row>
                            <div .span9>
                                <form method="POST" action="@{ProjectR project_id}">
                                    ^{hidden_form}
                                    <em>
                                        This is a preview.
                                    <br>
                                    <script>
                                        document.write('<input type="submit" value="edit" onclick="history.go(-1);return false;" />')
                                    <input type=submit name=mode value=update>
                    |]

                Just "update" -> do
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

                                tag_ids <- forM tags $ \ tag_name -> do
                                    tag_entity_list <- select $ from $ \ tag -> do
                                        where_ (tag ^. TagName ==. val tag_name)
                                        return tag

                                    case tag_entity_list of
                                        [] -> insert $ Tag tag_name
                                        (Entity tag_id _) : _ -> return tag_id


                                delete $ from $ \ project_tag -> do
                                    where_ (project_tag ^. ProjectTagProject ==. val project_id)

                                forM_ tag_ids $ \ tag_id -> insert $ ProjectTag project_id tag_id

                                return True

                    if processed
                     then do
                         setMessage "project updated"
                         redirect $ ProjectR project_id
                     else notFound

                _ -> do
                    setMessage "error: unrecognized mode"
                    redirect $ ProjectR project_id
        _ -> do
            setMessage "error"
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
