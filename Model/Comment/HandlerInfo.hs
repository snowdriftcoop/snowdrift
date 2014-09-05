module Model.Comment.HandlerInfo
    ( CommentHandlerInfo(..)
    , projectCommentHandlerInfo
    , wikiPageCommentHandlerInfo
    ) where

import Import

import Model.Comment.ActionPermissions
import Model.Comment.Mods
import Model.Comment.Routes
import Model.Comment.Sql

-- | Data type that packages together the necessary data for
-- handling a Comment page. The viewing permission, routes,
-- and action permissions vary depending on exactly where
-- the comment is.
data CommentHandlerInfo = CommentHandlerInfo
    { commentHandlerHasPermission            :: ExprCommentCond
    , commentHandlerRoutes                   :: CommentRoutes
    , commentHandlerMakeActionPermissionsMap :: MakeActionPermissionsMap
    }

projectCommentHandlerInfo :: Maybe (Entity User) -> ProjectId -> Text -> CommentMods -> CommentHandlerInfo
projectCommentHandlerInfo muser project_id project_handle mods =
    CommentHandlerInfo
        (exprCommentProjectPermissionFilter (entityKey <$> muser) (val project_id))
        (projectCommentRoutes project_handle)
        (makeProjectCommentActionPermissionsMap muser project_handle mods)

wikiPageCommentHandlerInfo :: Maybe (Entity User) -> ProjectId -> Text -> Text -> CommentMods -> CommentHandlerInfo
wikiPageCommentHandlerInfo muser project_id project_handle target mods =
    CommentHandlerInfo
        (exprCommentProjectPermissionFilter (entityKey <$> muser) (val project_id))
        (wikiPageCommentRoutes project_handle target)
        (makeProjectCommentActionPermissionsMap muser project_handle mods)
