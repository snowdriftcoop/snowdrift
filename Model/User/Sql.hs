module Model.User.Sql
  ( exprIsModerator
  , exprUserViewedComments
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


-- | Expression to get all the Comments a User has viewed.
exprUserViewedComments :: UserId -> SqlExpr (ValueList CommentId)
exprUserViewedComments user_id =
    subList_select $
    from $ \(c `InnerJoin` vc) -> do
    on_ (c ^. CommentId ==. vc ^. ViewCommentComment)
    where_ (vc ^. ViewCommentUser ==. val user_id)
    return (c ^. CommentId)
