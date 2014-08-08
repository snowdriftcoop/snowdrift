module Model.Project.Sql where

import Import

import Model.Comment.Sql
import Model.WikiPage.Sql

-- | Query that returns all Comments made on any WikiPage on this Project.
querProjectCommentIdsPostedOnWikiPagesDB :: ProjectId -> Maybe UserId -> SqlQuery (SqlExpr (Value CommentId))
querProjectCommentIdsPostedOnWikiPagesDB project_id muser_id =
    from $ \(c `InnerJoin` wp) -> do
    on_ (exprCommentOnWikiPage c wp)
    where_ $
        exprWikiPageOnProject wp project_id &&.
        exprPermissionFilter muser_id (val project_id) c
    return (c ^. CommentId)

-- | Query that returns all WikiEdits made on any WikiPage on this Project
querProjectWikiEdits :: ProjectId -> SqlQuery (SqlExpr (Value WikiEditId))
querProjectWikiEdits project_id =
    from $ \(wp `InnerJoin` we) -> do
    on_ (wp ^. WikiPageId ==. we ^. WikiEditPage)
    where_ (exprWikiPageOnProject wp project_id)
    return (we ^. WikiEditId)
