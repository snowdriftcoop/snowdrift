module Model.Project.Sql where

import Import

import Model.Comment.Sql
import Model.WikiPage.Sql

querProjectCommentIdsPostedOnWikiPagesDB :: ProjectId -> SqlQuery (SqlExpr (Value CommentId))
querProjectCommentIdsPostedOnWikiPagesDB project_id =
    from $ \(c `InnerJoin` wp) -> do
    on_ (exprCommentOnWikiPage c wp)
    where_ (exprWikiPageOnProject wp project_id)
    return (c ^. CommentId)
