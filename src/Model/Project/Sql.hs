module Model.Project.Sql where

import Import

import Model.Wiki.Sql

-- | Query that returns all WikiEdits made on any WikiPage on this Project
querProjectWikiEdits :: ProjectId -> SqlQuery (SqlExpr (Value WikiEditId))
querProjectWikiEdits project_id =
    from $ \(wp `InnerJoin` we) -> do
    on_ (wp ^. WikiPageId ==. we ^. WikiEditPage)
    where_ (exprWikiPageOnProject wp project_id)
    return (we ^. WikiEditId)
