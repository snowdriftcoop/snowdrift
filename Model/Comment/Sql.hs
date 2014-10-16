module Model.Comment.Sql
    ( ExprCommentCond
    , exprCommentApproved
    , exprCommentClosedOrRetracted
    , exprCommentFlagged
    , exprCommentIsRoot
    , exprCommentNotRethreaded
    , exprCommentOnDiscussion
    , exprCommentOnWikiPage
    , exprCommentOpen
    , exprCommentPostedBy
    , exprCommentUserPermissionFilter
    , exprCommentProjectPermissionFilter
    , exprCommentProjectPermissionFilterIncludingRethreaded
    , exprCommentRootPostedBy
    , exprCommentUnapproved
    , exprCommentViewedBy
    , querCommentAncestors
    , querCommentDescendants
    , querCommentsDescendants
    ) where

import Import

import Model.User.Sql

-- | A SQL expression for a comment "condition" (Comment -> Bool).
type ExprCommentCond = SqlExpr (Entity Comment) -> SqlExpr (Value Bool)

exprCommentClosedOrRetracted, exprCommentOpen :: ExprCommentCond
exprCommentClosedOrRetracted c = c ^. CommentId `in_`   exprClosedCommentIds ||. c ^. CommentId `in_`   exprRetractedCommentIds
exprCommentOpen              c = c ^. CommentId `notIn` exprClosedCommentIds &&. c ^. CommentId `notIn` exprRetractedCommentIds

exprClosedCommentIds :: SqlExpr (ValueList CommentId)
exprClosedCommentIds =
    subList_select $
    from $ \cc ->
    return (cc ^. CommentClosingComment)

exprRetractedCommentIds :: SqlExpr (ValueList CommentId)
exprRetractedCommentIds =
    subList_select $
    from $ \cr ->
    return (cr ^. CommentRetractingComment)

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

-- Is the root (earliest ancestor) of this comment posted by the given user?
exprCommentRootPostedBy :: UserId -> ExprCommentCond
exprCommentRootPostedBy user_id c = ((isNothing (c ^. CommentParent)) &&. c ^. CommentUser ==. val user_id) ||. c ^. CommentId `in_` sublist
  where
    sublist = subList_select $ from $ \ (comment_ancestor `InnerJoin` root) -> do
        on_ $ root ^. CommentId ==. comment_ancestor ^. CommentAncestorAncestor
        where_ $ isNothing (root ^. CommentParent)
            &&. root ^. CommentUser ==. val user_id
        return (comment_ancestor ^. CommentAncestorComment)

-- | SQL expression to filter a Comment (somewhere) on a Project based on "permissions", as follows:
--    If moderator, show all.
--    If logged in, show all approved (hiding flagged), plus own comments (unapproved + flagged).
--    If not logged in, show all approved (hiding flagged).
--    No matter what, hide rethreaded comments (they've essentially been replaced).
-- TODO(mitchell, david): rethink this function with regards to comment visibility
exprCommentProjectPermissionFilter :: Maybe UserId -> SqlExpr (Value ProjectId) -> ExprCommentCond
exprCommentProjectPermissionFilter muser_id project_id c =
    exprCommentNotRethreaded c &&. exprCommentProjectPermissionFilterIncludingRethreaded muser_id project_id c

-- | A "special case" of the above (almost universal) permission filter, for Project feeds: we *do*
-- want to display rethreaded Comments in this case, because otherwise, the original "comment posted"
-- feed events would vanish.
exprCommentProjectPermissionFilterIncludingRethreaded :: Maybe UserId -> SqlExpr (Value ProjectId) -> ExprCommentCond
exprCommentProjectPermissionFilterIncludingRethreaded muser_id project_id c = isVisible &&. permissionFilter
  where
    -- isVisible when comment is public (VisPublic), or the viewer is
    -- a project team member, or the viewer posted the topic initially
    -- TODO(mitchell, david): this is wrong, but good enough for now
    isVisible = case muser_id of
        Just user_id -> c ^. CommentVisibility ==. val VisPublic ||. exprUserIsTeamMember user_id project_id ||. exprCommentRootPostedBy user_id c
        Nothing -> c ^. CommentVisibility ==. val VisPublic

    permissionFilter = case muser_id of
        Just user_id -> approvedAndNotFlagged ||. exprCommentPostedBy user_id c ||. exprUserIsModerator user_id project_id
        Nothing      -> approvedAndNotFlagged

    approvedAndNotFlagged = exprCommentApproved c &&. not_ (exprCommentFlagged c)

-- | SQL expression to filter a Comment (somewhere) on a Project based on "permissions", as follows:
--    If moderator, show all.
--    If logged in, show all approved (hiding flagged), plus own comments (unapproved + flagged).
--    If not logged in, show all approved (hiding flagged).
--    No matter what, hide rethreaded comments (they've essentially been replaced).
-- TODO(mitchell, david): rethink this function with regards to comment visibility
exprCommentUserPermissionFilter :: Maybe UserId -> SqlExpr (Value UserId) -> ExprCommentCond
exprCommentUserPermissionFilter mviewer_id user_id c =
    exprCommentNotRethreaded c &&. exprCommentUserPermissionFilterIncludingRethreaded mviewer_id user_id c

-- | A "special case" of the above (almost universal) permission filter, for User feeds: we *do*
-- want to display rethreaded Comments in this case, because otherwise, the original "comment posted"
-- feed events would vanish.
exprCommentUserPermissionFilterIncludingRethreaded :: Maybe UserId -> SqlExpr (Value UserId) -> ExprCommentCond
exprCommentUserPermissionFilterIncludingRethreaded mviewer_id user_id c = isVisible &&. permissionFilter
  where
    -- isVisible when comment is public (VisPublic), or the viewer is
    -- the topic of discussion, or the viewer posted the topic initially
    -- TODO(mitchell, david): this is wrong, but good enough for now
    isVisible = case mviewer_id of
        Just viewer_id -> c ^. CommentVisibility ==. val VisPublic ||. val mviewer_id ==. just user_id ||. exprCommentRootPostedBy viewer_id c
        Nothing -> c ^. CommentVisibility ==. val VisPublic

    permissionFilter = case mviewer_id of
        Just viewer_id -> approvedAndNotFlagged ||. exprCommentPostedBy viewer_id c
        Nothing      -> approvedAndNotFlagged

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
