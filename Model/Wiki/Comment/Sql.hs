module Model.Wiki.Comment.Sql where

import Import

import Model.Comment.Sql
import Model.User.Sql

-- | SQL expression to filter a comment based on "permissions", as follows:
--    If moderator, show all.
--    If logged in, show all approved (hiding flagged), plus own comments (unapproved + flagged).
--    If not logged in, show all approved (hiding flagged).
--    No matter what, hide rethreaded comments (they've essentially been replaced).
exprCommentWikiPagePermissionFilter :: Maybe UserId -> SqlExpr (Value ProjectId) -> ExprCommentCond
exprCommentWikiPagePermissionFilter muser_id project_id c = exprCommentNotRethreaded c &&. permissionFilter
  where
    permissionFilter :: SqlExpr (Value Bool)
    permissionFilter = case muser_id of
        Just user_id -> approvedAndNotFlagged ||. exprCommentPostedBy user_id c ||. exprUserIsModerator user_id project_id
        Nothing      -> approvedAndNotFlagged

    approvedAndNotFlagged = exprCommentApproved c &&. not_ (exprCommentFlagged c)
