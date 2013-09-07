{-# LANGUAGE TupleSections #-}

module Handler.Project where

import Import

import Model.Currency
import Model.Project
import Model.Shares
import Model.Markdown.Diff
import Model.User

import qualified Data.Text as T
import qualified Data.Set as S

import Widgets.Sidebar
import Widgets.Markdown

import Yesod.Markdown
import Model.Markdown

lookupGetParamDefault :: Read a => Text -> a -> Handler a
lookupGetParamDefault name def = do
    maybe_value <- lookupGetParam name
    return $ fromMaybe def $ maybe_value >>= readMaybe . T.unpack


getProjectsR :: Handler Html
getProjectsR = do
    page <- lookupGetParamDefault "page" 0
    per_page <- lookupGetParamDefault "count" 20
    tags <- maybe [] (map T.strip . T.splitOn ",") <$> lookupGetParam "tags"
    projects <- runDB $ if null tags
        then selectList [] [ Asc ProjectCreatedTs, LimitTo per_page, OffsetBy page ]
        else do
            tagged_projects <- forM tags $ \ name -> select $ from $ \ (t `InnerJoin` p_t) -> do
                on_ (t ^. TagId ==. p_t ^. ProjectTagTag)
                where_ ( t ^. TagName ==. val name )
                return p_t

            let project_ids = if null tagged_projects then S.empty else foldl1 S.intersection $ map (S.fromList . map (projectTagProject . entityVal)) tagged_projects
            selectList [ ProjectId <-. S.toList project_ids ] [ Asc ProjectCreatedTs, LimitTo per_page, OffsetBy page ]

    defaultLayout $(widgetFile "projects")


getProjectR :: Text -> Handler Html
getProjectR project_handle = do
    maybe_viewer_id <- maybeAuthId

    (project, pledges, pledge) <- runDB $ do
        Entity project_id project <- getBy404 $ UniqueProjectHandle project_handle
        pledges <- getProjectShares project_id
        pledge <- case maybe_viewer_id of
            Nothing -> return Nothing
            Just viewer_id -> getBy $ UniquePledge viewer_id project_id

        return (project, pledges, pledge)

    defaultLayout $ renderProject (Just project_handle) project pledges pledge


renderProject :: Maybe Text
                 -> Project
                 -> [Int64]
                 -> Maybe (Entity Pledge)
                 -> WidgetT App IO ()
renderProject maybe_project_handle project pledges pledge = do
    let share_value = projectShareValue project
        users = fromIntegral $ length pledges
        shares = sum pledges
        project_value = share_value $* fromIntegral shares
        description = renderMarkdown (fromMaybe "???" maybe_project_handle) $ projectDescription project

        maybe_shares = pledgeShares . entityVal <$> pledge

    ((_, update_shares), _) <- handlerToWidget $ generateFormGet $ buySharesForm $ fromMaybe 0 maybe_shares

    $(widgetFile "project")


data UpdateProject = UpdateProject { updateProjectName :: Text, updateProjectDescription :: Markdown, updateProjectTags :: [Text] }


editProjectForm :: Maybe (Project, [Text]) -> Form UpdateProject
editProjectForm project =
    renderDivs $ UpdateProject
        <$> areq textField "Project Name" (projectName . fst <$> project)
        <*> areq snowdriftMarkdownField "Description" (projectDescription . fst <$> project)
        <*> (map T.strip . T.splitOn "," <$> areq textField "Tags" (T.intercalate ", " . snd <$> project))

previewProjectForm :: Maybe (Project, [Text]) -> Form UpdateProject
previewProjectForm project =
    renderDivs $ UpdateProject
        <$> areq hiddenField "" (projectName . fst <$> project)
        <*> (Markdown <$> areq hiddenField "" ((\ (Markdown str) -> str) . projectDescription . fst <$> project))
        <*> (map T.strip . T.splitOn "," <$> areq hiddenField "" (T.intercalate ", " . snd <$> project))


getEditProjectR :: Text -> Handler Html
getEditProjectR project_handle = do
    viewer_id <- requireAuthId 

    Entity project_id project <- runDB $ do
        can_edit <- (||) <$> isProjectAdmin project_handle viewer_id <*> isProjectAdmin "snowdrift" viewer_id
        if can_edit
         then getBy404 $ UniqueProjectHandle project_handle
         else permissionDenied "You do not have permission to edit this project."

    tags <- runDB $ select $ from $ \ (p_t `InnerJoin` tag) -> do
        on_ (p_t ^. ProjectTagTag ==. tag ^. TagId)
        where_ (p_t ^. ProjectTagProject ==. val project_id)
        return tag

    (project_form, _) <- generateFormPost $ editProjectForm (Just (project, map (tagName . entityVal) tags))

    defaultLayout $(widgetFile "edit_project")


postProjectR :: Text -> Handler Html
postProjectR project_handle = do
    viewer_id <- requireAuthId

    Entity project_id project <- runDB $ do
        can_edit <- (||) <$> isProjectAdmin project_handle viewer_id <*> isProjectAdmin "snowdrift" viewer_id
        if can_edit
         then getBy404 $ UniqueProjectHandle project_handle
         else permissionDenied "You do not have permission to edit this project."

    ((result, _), _) <- runFormPost $ editProjectForm Nothing

    now <- liftIO getCurrentTime

    case result of
        FormSuccess (UpdateProject name description tags) -> do
            mode <- lookupPostParam "mode"
            let action :: Text = "update"
            case mode of
                Just "preview" -> do
                    let preview_project = project { projectName = name, projectDescription = description }
                    (hidden_form, _) <- generateFormPost $ previewProjectForm $ Just (preview_project, tags)
                    let rendered_project = renderProject (Just project_handle) preview_project [] Nothing
                        preview_controls = [whamlet|
                            <div .row>
                                <div .span9>
                                    <form method="POST" action="@{ProjectR project_handle}">
                                        ^{hidden_form}
                                        <div .alert>
                                            This is a preview; your changes are not yet saved!
                                        <script>
                                            document.write('<input type="submit" value="edit" onclick="history.go(-1);return false;" />')
                                        <input type=submit name=mode value=#{action}>
                        |]

                    defaultLayout $ [whamlet|
                        ^{preview_controls}
                        ^{rendered_project}
                        ^{preview_controls}
                    |]

                Just x | x == action -> do
                    runDB $ do
                        when (projectDescription project /= description) $ do
                            project_update <- insert $ ProjectUpdate now project_id viewer_id $ diffMarkdown (projectDescription project) description
                            last_update <- getBy $ UniqueProjectLastUpdate project_id
                            case last_update of
                                Just (Entity key _) -> repsert key $ ProjectLastUpdate project_id project_update
                                Nothing -> void $ insert $ ProjectLastUpdate project_id project_update

                        update $ \ p -> do
                            set p [ ProjectName =. val name, ProjectDescription =. val description ]
                            where_ (p ^. ProjectId ==. val project_id)

                        tag_ids <- forM tags $ \ tag_name -> do
                            tag_entity_list <- select $ from $ \ tag -> do
                                where_ (tag ^. TagName ==. val tag_name)
                                return tag

                            case tag_entity_list of
                                [] -> insert $ Tag tag_name
                                Entity tag_id _ : _ -> return tag_id


                        delete $ from $ \ project_tag -> where_ (project_tag ^. ProjectTagProject ==. val project_id)

                        forM_ tag_ids $ \ tag_id -> insert $ ProjectTag project_id tag_id

                    setMessage "project updated"
                    redirect $ ProjectR project_handle

                _ -> do
                    setMessage "error: unrecognized mode"
                    redirect $ ProjectR project_handle
        _ -> do
            setMessage "error"
            redirect $ ProjectR project_handle


getProjectPatronsR :: Text -> Handler Html
getProjectPatronsR project_handle = do
    _ <- requireAuthId

    page <- lookupGetParamDefault "page" 0
    per_page <- lookupGetParamDefault "count" 20

    (project, pledges) <- runDB $ do
        Entity project_id project <- getBy404 $ UniqueProjectHandle project_handle
        pledges <- select $ from $ \(pledge `InnerJoin` user) -> do
            on_ (pledge ^. PledgeUser ==. user ^. UserId)
            where_ (pledge ^. PledgeProject ==. val project_id)
            orderBy [ desc (pledge ^. PledgeShares), asc (user ^. UserName), asc (user ^. UserId)]
            offset page
            limit per_page
            return (pledge, user)

        return (project, pledges)

    defaultLayout $(widgetFile "project_donors")
