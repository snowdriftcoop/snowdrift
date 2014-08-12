module Model.Wiki.Comment where

import Import

import Model.Comment
import Model.User

-- | Make a CommentActionPermissions for displaying a WikiPage's Comment tree
-- (this function, partially applied, is passed to View.Comment.makeCommentTreeWidget)
makeWikiPageCommentActionPermissions :: Text -> Text -> Entity Comment -> Handler CommentActionPermissions
makeWikiPageCommentActionPermissions project_handle target comment_entity@(Entity comment_id comment) = do

    Just current_route <- getCurrentRoute
    let is_reply_route = current_route == ReplyWikiCommentR project_handle target comment_id

    maybeAuth >>= \case
        Nothing -> return $ CommentActionPermissions
            { can_add_tag   = False
            , can_approve   = False
            , can_close     = False
            , can_delete    = False
            , can_edit      = False
            , can_establish = False
            , can_flag      = False
            , can_reply     = not is_reply_route
            , can_rethread  = False
            , can_retract   = False
            }
        Just (Entity viewer_id viewer) -> do
            let poster_id = commentUser comment
            (poster, is_mod, can_del, is_flagged) <- runYDB $ do
                Entity project_id _ <- getBy404 (UniqueProjectHandle project_handle)
                (,,,) <$> get404 poster_id
                      <*> userIsProjectModeratorDB viewer_id project_id
                      <*> userCanDeleteCommentDB viewer_id comment_entity
                      <*> commentIsFlagged comment_id
            return $ CommentActionPermissions
                { can_add_tag   = userIsEstablished viewer
                , can_approve   = is_mod && (not . commentIsApproved) comment
                , can_close     = userIsEstablished viewer
                , can_delete    = can_del
                , can_edit      = userCanEditComment viewer_id comment
                , can_establish = is_mod && userIsUnestablished poster
                , can_flag      = userIsEstablished viewer && viewer_id /= poster_id && not is_flagged
                , can_reply     = not is_reply_route && not is_flagged
                , can_rethread  = poster_id == viewer_id || is_mod
                , can_retract   = poster_id == viewer_id
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
