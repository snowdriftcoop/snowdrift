module Model.Project.Sql where

import Import

import Model.Comment.Sql
import Model.Wiki.Sql

querProjectCommentsDB :: ProjectId -> Maybe UserId -> SqlQuery (SqlExpr (Value CommentId))
querProjectCommentsDB project_id muser_id =
    from $ \c -> do
    -- Add more locations for Comments here as necessary.
    where_ (c ^. CommentId `in_` subList_select (querProjectCommentsOnWikiPagesDB project_id muser_id))
    return (c ^. CommentId)

querProjectCommentsOnWikiPagesDB :: ProjectId -> Maybe UserId -> SqlQuery (SqlExpr (Value CommentId))
querProjectCommentsOnWikiPagesDB project_id muser_id =
    from $ \(c `InnerJoin` wp) -> do
    on_ (exprCommentOnWikiPage c wp)
    where_ $
        exprWikiPageOnProject wp project_id &&.
        exprCommentProjectPermissionFilter muser_id (val project_id) c
    return (c ^. CommentId)

-- | Query that returns all WikiEdits made on any WikiPage on this Project
querProjectWikiEdits :: ProjectId -> SqlQuery (SqlExpr (Value WikiEditId))
querProjectWikiEdits project_id =
    from $ \(wp `InnerJoin` we) -> do
    on_ (wp ^. WikiPageId ==. we ^. WikiEditPage)
    where_ (exprWikiPageOnProject wp project_id)
    return (we ^. WikiEditId)
