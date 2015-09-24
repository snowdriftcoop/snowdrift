module Model.Comment.Routes where

import Import

-- | Collection of Routes that can be made from a CommentId (because
-- Comments on various places around the site have different URLs, even
-- though their IDs are unique).
data CommentRoutes = CommentRoutes
    { comment_route_add_tag   :: CommentId -> Route App
    , comment_route_approve   :: CommentId -> Route App
    , comment_route_close     :: CommentId -> Route App
    , comment_route_claim     :: CommentId -> Route App
    , comment_route_delete    :: CommentId -> Route App
    , comment_route_edit      :: CommentId -> Route App
    , comment_route_flag      :: CommentId -> Route App
    , comment_route_permalink :: CommentId -> Route App
    , comment_route_reply     :: CommentId -> Route App
    , comment_route_rethread  :: CommentId -> Route App
    , comment_route_retract   :: CommentId -> Route App
    , comment_route_tag       :: CommentId -> TagId -> Route App
    , comment_route_unclaim   :: CommentId -> Route App
    , comment_route_watch     :: CommentId -> Route App
    , comment_route_unwatch   :: CommentId -> Route App
    }

dummyCommentRoutes :: CommentRoutes
dummyCommentRoutes = CommentRoutes
    (const HomeR)
    (const HomeR)
    (const HomeR)
    (const HomeR)
    (const HomeR)
    (const HomeR)
    (const HomeR)
    (const HomeR)
    (const HomeR)
    (const HomeR)
    (const HomeR)
    (\_ _ -> HomeR)
    (const HomeR)
    (const HomeR)
    (const HomeR)

projectCommentRoutes :: Text -> CommentRoutes
projectCommentRoutes project_handle = CommentRoutes
    { comment_route_add_tag   = ProjectCommentAddTagR   project_handle
    , comment_route_approve   = ApproveProjectCommentR  project_handle
    , comment_route_claim     = ClaimProjectCommentR    project_handle
    , comment_route_close     = CloseProjectCommentR    project_handle
    , comment_route_delete    = DeleteProjectCommentR   project_handle
    , comment_route_edit      = EditProjectCommentR     project_handle
    , comment_route_flag      = FlagProjectCommentR     project_handle
    , comment_route_permalink = ProjectCommentR         project_handle
    , comment_route_reply     = ReplyProjectCommentR    project_handle
    , comment_route_rethread  = RethreadProjectCommentR project_handle
    , comment_route_retract   = RetractProjectCommentR  project_handle
    , comment_route_tag       = ProjectCommentTagR      project_handle
    , comment_route_unclaim   = UnclaimProjectCommentR  project_handle
    , comment_route_watch     = WatchProjectCommentR    project_handle
    , comment_route_unwatch   = UnwatchProjectCommentR  project_handle
    }

blogPostCommentRoutes :: Text -> Text -> CommentRoutes
blogPostCommentRoutes project_handle post_name = CommentRoutes
    { comment_route_add_tag
        = BlogPostCommentAddTagR   project_handle post_name
    , comment_route_approve
        = ApproveBlogPostCommentR  project_handle post_name
    , comment_route_claim
        = ClaimBlogPostCommentR    project_handle post_name
    , comment_route_close
        = CloseBlogPostCommentR    project_handle post_name
    , comment_route_delete
        = DeleteBlogPostCommentR   project_handle post_name
    , comment_route_edit
        = EditBlogPostCommentR     project_handle post_name
    , comment_route_flag
        = FlagBlogPostCommentR     project_handle post_name
    , comment_route_permalink
        = BlogPostCommentR         project_handle post_name
    , comment_route_reply
        = ReplyBlogPostCommentR    project_handle post_name
    , comment_route_rethread
        = RethreadBlogPostCommentR project_handle post_name
    , comment_route_retract
        = RetractBlogPostCommentR  project_handle post_name
    , comment_route_tag
        = BlogPostCommentTagR      project_handle post_name
    , comment_route_unclaim
        = UnclaimBlogPostCommentR  project_handle post_name
    , comment_route_watch
        = WatchBlogPostCommentR    project_handle post_name
    , comment_route_unwatch
        = UnwatchBlogPostCommentR  project_handle post_name
    }

wikiPageCommentRoutes :: Text -> Language -> Text -> CommentRoutes
wikiPageCommentRoutes project_handle language target = CommentRoutes
    { comment_route_add_tag
        = WikiCommentAddTagR   project_handle language target
    , comment_route_approve
        = ApproveWikiCommentR  project_handle language target
    , comment_route_claim
        = ClaimWikiCommentR    project_handle language target
    , comment_route_close
        = CloseWikiCommentR    project_handle language target
    , comment_route_delete
        = DeleteWikiCommentR   project_handle language target
    , comment_route_edit
        = EditWikiCommentR     project_handle language target
    , comment_route_flag
        = FlagWikiCommentR     project_handle language target
    , comment_route_permalink
        = WikiCommentR         project_handle language target
    , comment_route_reply
        = ReplyWikiCommentR    project_handle language target
    , comment_route_rethread
        = RethreadWikiCommentR project_handle language target
    , comment_route_retract
        = RetractWikiCommentR  project_handle language target
    , comment_route_tag
        = WikiCommentTagR      project_handle language target
    , comment_route_unclaim
        = UnclaimWikiCommentR  project_handle language target
    , comment_route_watch
        = WatchWikiCommentR    project_handle language target
    , comment_route_unwatch
        = UnwatchWikiCommentR  project_handle language target
    }

userCommentRoutes :: UserId -> CommentRoutes
userCommentRoutes user_id = CommentRoutes
    { comment_route_add_tag   = UserCommentAddTagR   user_id
    , comment_route_approve   = ApproveUserCommentR  user_id
    , comment_route_claim     = ClaimUserCommentR    user_id
    , comment_route_close     = CloseUserCommentR    user_id
    , comment_route_delete    = DeleteUserCommentR   user_id
    , comment_route_edit      = EditUserCommentR     user_id
    , comment_route_flag      = FlagUserCommentR     user_id
    , comment_route_permalink = UserCommentR         user_id
    , comment_route_reply     = ReplyUserCommentR    user_id
    , comment_route_rethread  = RethreadUserCommentR user_id
    , comment_route_retract   = RetractUserCommentR  user_id
    , comment_route_tag       = UserCommentTagR      user_id
    , comment_route_unclaim   = UnclaimUserCommentR  user_id
    , comment_route_watch     = WatchUserCommentR    user_id
    , comment_route_unwatch   = UnwatchUserCommentR  user_id
    }
