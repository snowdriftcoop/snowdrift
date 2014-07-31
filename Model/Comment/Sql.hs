module Model.Comment.Sql where

import Import

import Model.User.Sql (exprIsModerator)

exprClosed, exprOpen :: SqlExpr (Entity Comment) -> SqlExpr (Value Bool)
exprClosed c = c ^. CommentId `in_`   exprClosedCommentIds
exprOpen   c = c ^. CommentId `notIn` exprClosedCommentIds

exprClosedCommentIds :: SqlExpr (ValueList CommentId)
exprClosedCommentIds =
    subList_select $
    from $ \cl ->
    return (cl ^. CommentClosureComment)

-- | Comment is root?
exprRoot :: SqlExpr (Entity Comment) -> SqlExpr (Value Bool)
exprRoot c = isNothing (c ^. CommentParent)

-- | Comment on this Discussion?
exprOnDiscussion :: DiscussionId -> SqlExpr (Entity Comment) -> SqlExpr (Value Bool)
exprOnDiscussion discussion_id c = c ^. CommentDiscussion ==. val discussion_id

-- | SQL expression to filter a comment based on "permissions", as follows:
--    If moderator, show all.
--    If logged in, show all approved (hiding flagged), plus own comments (unapproved + flagged).
--    If not logged in, show all approved (hiding flagged).
--    No matter what, hide rethreaded comments (they've essentially been replaced).
--
-- The logic here is DUPLICATED (in Haskell land) in Handler.Wiki.Comment.checkCommentPage
-- (because that function only fetches the root comment via Database.Persist.get) - all
-- changes here must be reflected there, too!
exprPermissionFilter :: Maybe UserId
                     -> SqlExpr (Value ProjectId)
                     -> SqlExpr (Entity Comment)
                     -> SqlExpr (Value Bool)
exprPermissionFilter muser_id project_id c = exprNotRethreaded c &&. permissionFilter
  where
    permissionFilter :: SqlExpr (Value Bool)
    permissionFilter = case muser_id of
        Just user_id -> exprApprovedAndNotFlagged c ||. exprPostedBy user_id c ||. exprIsModerator user_id project_id
        Nothing      -> exprApprovedAndNotFlagged c

exprNotRethreaded :: SqlExpr (Entity Comment) -> SqlExpr (Value Bool)
exprNotRethreaded c = c ^. CommentId `notIn` rethreadedCommentIds
  where
    rethreadedCommentIds :: SqlExpr (ValueList CommentId)
    rethreadedCommentIds =
        subList_select $
        from $ \r ->
        return (r ^. RethreadOldComment)

exprApproved :: SqlExpr (Entity Comment) -> SqlExpr (Value Bool)
exprApproved = not_ . exprUnapproved

exprUnapproved :: SqlExpr (Entity Comment) -> SqlExpr (Value Bool)
exprUnapproved c = isNothing (c ^. CommentModeratedTs)

exprNotFlagged :: SqlExpr (Entity Comment) -> SqlExpr (Value Bool)
exprNotFlagged c = c ^. CommentId `notIn` flaggedCommentIds
  where
    flaggedCommentIds :: SqlExpr (ValueList CommentId)
    flaggedCommentIds =
        subList_select $
        from $ \cf ->
        return (cf ^. CommentFlaggingComment)

exprApprovedAndNotFlagged :: SqlExpr (Entity Comment) -> SqlExpr (Value Bool)
exprApprovedAndNotFlagged c = exprApproved c &&. exprNotFlagged c

exprPostedBy :: UserId -> SqlExpr (Entity Comment) -> SqlExpr (Value Bool)
exprPostedBy user_id c = c ^. CommentUser ==. val user_id

exprCommentViewedBy :: UserId -> SqlExpr (Entity Comment) -> SqlExpr (Value Bool)
exprCommentViewedBy user_id c = c ^. CommentId `in_`
    (subList_select $
     from $ \vc -> do
     where_ (vc ^. ViewCommentUser ==. val user_id)
     return (vc ^. ViewCommentComment))

querAncestors :: CommentId -> SqlQuery (SqlExpr (Value CommentId))
querAncestors comment_id =
    from $ \ca -> do
    where_ (ca ^. CommentAncestorComment ==. val comment_id)
    return (ca ^. CommentAncestorAncestor)

querDescendants :: CommentId -> SqlQuery (SqlExpr (Value CommentId))
querDescendants comment_id =
    from $ \ca -> do
    where_ (ca ^. CommentAncestorAncestor ==. val comment_id)
    return (ca ^. CommentAncestorComment)

querAllDescendants :: [CommentId] -> SqlQuery (SqlExpr (Value CommentId))
querAllDescendants comment_ids =
    from $ \ca -> do
    where_ (ca ^. CommentAncestorAncestor `in_` valList comment_ids)
    return (ca ^. CommentAncestorComment)
