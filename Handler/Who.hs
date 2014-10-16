module Handler.Who where

import Import


import Data.List (sortBy)

import Model.Markdown
import Model.Discussion

userShortName :: User -> Text
userShortName user = fromMaybe (userIdent user) $ userName user

getWhoR :: Text -> Handler Html
getWhoR project_handle = do
    Entity project_id project <- runYDB $ getBy404 $ UniqueProjectHandle project_handle
    team_members <- runDB $
        select $
        from $ \ (user `InnerJoin` project_user_role) -> do
        on_ $ user ^. UserId ==. project_user_role ^. ProjectUserRoleUser
        where_ $ (project_user_role ^. ProjectUserRoleProject ==. val project_id)
             &&. (project_user_role ^. ProjectUserRoleRole ==. val TeamMember)
        return user

    let members = sortBy (compare `on` (userCreatedTs . entityVal)) team_members
        discussion = DiscussionOnProject $ Entity project_id project

    defaultLayout $ do
        setTitle . toHtml $ projectName project <> " - Team | Snowdrift.coop"
        $(widgetFile "who")
