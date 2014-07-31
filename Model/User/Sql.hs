module Model.User.Sql
  ( exprIsModerator
  ) where

import Import

exprIsModerator :: UserId -> SqlExpr (Value ProjectId) -> SqlExpr (Value Bool)
exprIsModerator = exprHasRole Moderator

exprHasRole :: Role -> UserId -> SqlExpr (Value ProjectId) -> SqlExpr (Value Bool)
exprHasRole role user_id project_id =
    exists $
    from $ \r -> do
    where_ $
        r ^. ProjectUserRoleProject  ==. project_id  &&.
        r ^. ProjectUserRoleUser     ==. val user_id &&.
        r ^. ProjectUserRoleRole     ==. val role
