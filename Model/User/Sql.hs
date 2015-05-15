module Model.User.Sql
  ( exprUserIsModerator
  , exprUserIsTeamMember
  , exprUserViewedComments
  , exprUserViewedWikiEdits
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


-- | Expression to get all the Comments a User has viewed.
exprUserViewedComments :: UserId -> SqlExpr (ValueList CommentId)
exprUserViewedComments user_id =
    subList_select $
    from $ \(c `InnerJoin` vc) -> do
    on_ (c ^. CommentId ==. vc ^. ViewCommentComment)
    where_ (vc ^. ViewCommentUser ==. val user_id)
    return (c ^. CommentId)

-- | Expression to get all the WikiEdits a User has viewed.
exprUserViewedWikiEdits :: UserId -> SqlExpr (ValueList WikiEditId)
exprUserViewedWikiEdits user_id =
    subList_select $
    from $ \vwe -> do
    where_ (vwe ^. ViewWikiEditUser ==. val user_id)
    return (vwe ^. ViewWikiEditEdit)
