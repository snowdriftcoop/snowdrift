module Model.User.Sql
  ( exprUserIsModerator
  , exprUserIsTeamMember
  ) where

import Import

exprUserIsModerator
    :: UserId
    -> SqlExpr (Value ProjectId)
    -> SqlExpr (Value Bool)
exprUserIsModerator = exprHasRole Moderator

exprUserIsTeamMember
    :: UserId
    -> SqlExpr (Value ProjectId)
    -> SqlExpr (Value Bool)
exprUserIsTeamMember = exprHasRole TeamMember

exprHasRole
    :: Role
    -> UserId
    -> SqlExpr (Value ProjectId)
    -> SqlExpr (Value Bool)
exprHasRole role user_id project_id =
    exists $
    from $ \r ->
    where_ $
        r ^. ProjectUserRoleProject  ==. project_id  &&.
        r ^. ProjectUserRoleUser     ==. val user_id &&.
        r ^. ProjectUserRoleRole     ==. val role
