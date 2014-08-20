module Model.Comment.ActionPermissions
    ( CommentActionPermissions(..)
    , MakeCommentActionPermissions
    , makeProjectCommentActionPermissions
    , makeWikiPageCommentActionPermissions
    ) where

import Import

import Model.Comment
import Model.User

type MakeCommentActionPermissions = Entity Comment -> Handler CommentActionPermissions

data CommentActionPermissions = CommentActionPermissions
    { can_add_tag   :: Bool
    , can_approve   :: Bool
    , can_close     :: Bool
    , can_delete    :: Bool
    , can_edit      :: Bool
    , can_establish :: Bool
    , can_flag      :: Bool
    , can_reply     :: Bool
    , can_rethread  :: Bool
    , can_retract   :: Bool
    }

-- | Make a CommentActionPermissions for displaying a WikiPage discussion's Comment tree.
makeWikiPageCommentActionPermissions :: Text -> Text -> MakeCommentActionPermissions
makeWikiPageCommentActionPermissions project_handle target comment@(Entity comment_id _) =
    makeProjectCommentActionPermissions' project_handle (ReplyWikiCommentR project_handle target comment_id) comment

-- | Make a CommentActionPermissions for displaying a Project discussion's Comment tree.
makeProjectCommentActionPermissions :: Text -> MakeCommentActionPermissions
makeProjectCommentActionPermissions project_handle comment@(Entity comment_id _) =
    makeProjectCommentActionPermissions' project_handle (ReplyProjectCommentR project_handle comment_id) comment

-- | Abstract Comment action permissions for Project discussion pages and WikiPage discussion pages,
-- which differ only in their reply permissions (different reply route).
makeProjectCommentActionPermissions' :: Text -> Route App -> MakeCommentActionPermissions
makeProjectCommentActionPermissions' project_handle reply_route comment_entity@(Entity comment_id comment) = do
    Just current_route <- getCurrentRoute
    let is_reply_route = current_route == reply_route

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
                , can_close     = userCanCloseComment viewer
                , can_delete    = can_del
                , can_edit      = userCanEditComment viewer_id comment
                , can_establish = is_mod && userIsUnestablished poster
                , can_flag      = userIsEstablished viewer && viewer_id /= poster_id && not is_flagged
                , can_reply     = not is_reply_route && not is_flagged
                , can_rethread  = poster_id == viewer_id || is_mod
                , can_retract   = poster_id == viewer_id
                }
