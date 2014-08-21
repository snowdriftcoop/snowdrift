module Model.Comment.HandlerInfo where

import Import

import Model.Comment.ActionPermissions
import Model.Comment.Routes
import Model.Comment.Sql

-- | Data type that packages together the necessary data for
-- handling a Comment page. The viewing permission, routes,
-- and action permissions vary depending on exactly where
-- the comment is.
data CommentHandlerInfo = CommentHandlerInfo
    { commentHandlerHasPermission         :: ExprCommentCond
    , commentHandlerRoutes                :: CommentRoutes
    , commentHandlerMakeActionPermissions :: MakeCommentActionPermissions
    }

projectCommentHandlerInfo :: Maybe UserId -> ProjectId -> Text -> CommentHandlerInfo
projectCommentHandlerInfo muser_id project_id project_handle = CommentHandlerInfo
    { commentHandlerHasPermission         = exprCommentProjectPermissionFilter muser_id (val project_id)
    , commentHandlerRoutes                = projectCommentRoutes project_handle
    , commentHandlerMakeActionPermissions = makeProjectCommentActionPermissions project_handle
    }

wikiPageCommentHandlerInfo :: Maybe UserId -> ProjectId -> Text -> Text -> CommentHandlerInfo
wikiPageCommentHandlerInfo muser_id project_id project_handle target = CommentHandlerInfo
    { commentHandlerHasPermission         = exprCommentProjectPermissionFilter muser_id (val project_id)
    , commentHandlerRoutes                = wikiPageCommentRoutes project_handle target
    , commentHandlerMakeActionPermissions = makeProjectCommentActionPermissions project_handle
    }
