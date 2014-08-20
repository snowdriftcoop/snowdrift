module Model.Comment.Routes where

import Import

-- | Collection of Routes that can be made from a CommentId (because Comments on
-- various places around the site have different URLs, even though their
-- IDs are unique).
data CommentRoutes = CommentRoutes
    { comment_route_add_tag   :: CommentId -> Route App
    , comment_route_approve   :: CommentId -> Route App
    , comment_route_close     :: CommentId -> Route App
    , comment_route_delete    :: CommentId -> Route App
    , comment_route_edit      :: CommentId -> Route App
    , comment_route_flag      :: CommentId -> Route App
    , comment_route_permalink :: CommentId -> Route App
    , comment_route_reply     :: CommentId -> Route App
    , comment_route_rethread  :: CommentId -> Route App
    , comment_route_retract   :: CommentId -> Route App
    , comment_route_tag       :: CommentId -> TagId -> Route App
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
    (\_ _ -> HomeR)

projectCommentRoutes :: Text -> CommentRoutes
projectCommentRoutes project_handle = CommentRoutes
    { comment_route_add_tag   = ProjectCommentAddTagR   project_handle
    , comment_route_approve   = ApproveProjectCommentR  project_handle
    , comment_route_close     = CloseProjectCommentR    project_handle
    , comment_route_delete    = DeleteProjectCommentR   project_handle
    , comment_route_edit      = EditProjectCommentR     project_handle
    , comment_route_flag      = FlagProjectCommentR     project_handle
    , comment_route_permalink = ProjectCommentR         project_handle
    , comment_route_reply     = ReplyProjectCommentR    project_handle
    , comment_route_rethread  = RethreadProjectCommentR project_handle
    , comment_route_retract   = RetractProjectCommentR  project_handle
    , comment_route_tag       = ProjectCommentTagR      project_handle
    }

wikiPageCommentRoutes :: Text -> Text -> CommentRoutes
wikiPageCommentRoutes project_handle target = CommentRoutes
    { comment_route_add_tag   = WikiCommentAddTagR   project_handle target
    , comment_route_approve   = ApproveWikiCommentR  project_handle target
    , comment_route_close     = CloseWikiCommentR    project_handle target
    , comment_route_delete    = DeleteWikiCommentR   project_handle target
    , comment_route_edit      = EditWikiCommentR     project_handle target
    , comment_route_flag      = FlagWikiCommentR     project_handle target
    , comment_route_permalink = WikiCommentR         project_handle target
    , comment_route_reply     = ReplyWikiCommentR    project_handle target
    , comment_route_rethread  = RethreadWikiCommentR project_handle target
    , comment_route_retract   = RetractWikiCommentR  project_handle target
    , comment_route_tag       = WikiCommentTagR      project_handle target
    }
