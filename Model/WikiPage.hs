module Model.WikiPage where

import Import

type PageInfo = (Entity Project, Entity WikiPage)

getPageInfo :: Text -> Text -> Handler PageInfo
getPageInfo project_handle target = runDB $ do
    project <- getBy404 $ UniqueProjectHandle project_handle
    page <- getBy404 $ UniqueWikiTarget (entityKey project) target

    return (project, page)
