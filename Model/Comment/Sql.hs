module Model.Comment.Sql where

import Import

import Model.User.Sql

type ExprCommentCond = SqlExpr (Entity Comment) -> SqlExpr (Value Bool)

exprCommentClosed, exprCommentOpen :: ExprCommentCond
exprCommentClosed c = c ^. CommentId `in_`   exprClosedCommentIds
exprCommentOpen   c = c ^. CommentId `notIn` exprClosedCommentIds

exprClosedCommentIds :: SqlExpr (ValueList CommentId)
exprClosedCommentIds =
    subList_select $
    from $ \cl ->
    return (cl ^. CommentClosureComment)

-- | Comment is root?
exprCommentIsRoot :: ExprCommentCond
exprCommentIsRoot c = isNothing (c ^. CommentParent)

-- | Comment on this Discussion?
exprCommentOnDiscussion :: DiscussionId -> ExprCommentCond
exprCommentOnDiscussion discussion_id c = c ^. CommentDiscussion ==. val discussion_id

-- | Comment on this WikiPage?
exprCommentOnWikiPage :: SqlExpr (Entity Comment) -> SqlExpr (Entity WikiPage) -> SqlExpr (Value Bool)
exprCommentOnWikiPage c wp = c ^. CommentDiscussion ==. wp ^. WikiPageDiscussion

exprCommentNotRethreaded :: ExprCommentCond
exprCommentNotRethreaded c = c ^. CommentId `notIn` rethreadedCommentIds
  where
    rethreadedCommentIds :: SqlExpr (ValueList CommentId)
    rethreadedCommentIds =
        subList_select $
        from $ \r ->
        return (r ^. RethreadOldComment)

exprCommentApproved :: ExprCommentCond
exprCommentApproved = not_ . exprCommentUnapproved

exprCommentUnapproved :: ExprCommentCond
exprCommentUnapproved c = isNothing (c ^. CommentApprovedTs)

exprCommentFlagged :: ExprCommentCond
exprCommentFlagged c = c ^. CommentId `in_` flaggedCommentIds
  where
    flaggedCommentIds :: SqlExpr (ValueList CommentId)
    flaggedCommentIds =
        subList_select $
        from $ \cf ->
        return (cf ^. CommentFlaggingComment)

exprCommentPostedBy :: UserId -> ExprCommentCond
exprCommentPostedBy user_id c = c ^. CommentUser ==. val user_id

exprCommentViewedBy :: UserId -> ExprCommentCond
exprCommentViewedBy user_id c = c ^. CommentId `in_`
    (subList_select $
     from $ \vc -> do
     where_ (vc ^. ViewCommentUser ==. val user_id)
     return (vc ^. ViewCommentComment))

-- | SQL expression to filter a Comment (somewhere) on a Project based on "permissions", as follows:
--    If moderator, show all.
--    If logged in, show all approved (hiding flagged), plus own comments (unapproved + flagged).
--    If not logged in, show all approved (hiding flagged).
--    No matter what, hide rethreaded comments (they've essentially been replaced).
exprCommentProjectPermissionFilter :: Maybe UserId -> SqlExpr (Value ProjectId) -> ExprCommentCond
exprCommentProjectPermissionFilter muser_id project_id c = exprCommentNotRethreaded c &&. permissionFilter
  where
    permissionFilter :: SqlExpr (Value Bool)
    permissionFilter = case muser_id of
        Just user_id -> approvedAndNotFlagged ||. exprCommentPostedBy user_id c ||. exprUserIsModerator user_id project_id
        Nothing      -> approvedAndNotFlagged

    approvedAndNotFlagged :: SqlExpr (Value Bool)
    approvedAndNotFlagged = exprCommentApproved c &&. not_ (exprCommentFlagged c)

querCommentAncestors :: CommentId -> SqlQuery (SqlExpr (Value CommentId))
querCommentAncestors comment_id =
    from $ \ca -> do
    where_ (ca ^. CommentAncestorComment ==. val comment_id)
    return (ca ^. CommentAncestorAncestor)

querCommentDescendants :: CommentId -> SqlQuery (SqlExpr (Value CommentId))
querCommentDescendants comment_id =
    from $ \ca -> do
    where_ (ca ^. CommentAncestorAncestor ==. val comment_id)
    return (ca ^. CommentAncestorComment)

querCommentsDescendants :: [CommentId] -> SqlQuery (SqlExpr (Value CommentId))
querCommentsDescendants comment_ids =
    from $ \ca -> do
    where_ (ca ^. CommentAncestorAncestor `in_` valList comment_ids)
    return (ca ^. CommentAncestorComment)
