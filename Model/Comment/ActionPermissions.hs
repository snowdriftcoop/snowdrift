module Model.Comment.ActionPermissions
    ( ActionPermissionsMap
    , CommentActionPermissions(..)
    , MakeActionPermissionsMap
    , makeProjectCommentActionPermissionsMap
    ) where

import Import

import Model.Comment
import Model.User

import qualified Data.Map      as M
import qualified Data.Set      as S

type MakeActionPermissionsMap = [Entity Comment] -> Handler ActionPermissionsMap
type ActionPermissionsMap = Map CommentId CommentActionPermissions

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

emptyCommentActionPermissions :: CommentActionPermissions
emptyCommentActionPermissions = CommentActionPermissions False False False False False False False False False False

-- | Comment action permissions for a logged out user.
loggedOutCommentActionPermissions :: CommentActionPermissions
loggedOutCommentActionPermissions = emptyCommentActionPermissions { can_reply = True }

makeLoggedOutCommentActionPermissionsMap :: MakeActionPermissionsMap
makeLoggedOutCommentActionPermissionsMap = return .
    foldr (\(Entity comment_id _) -> M.insert comment_id loggedOutCommentActionPermissions) mempty

-- | Action permissions that apply to both a Project discussion and a Projects WikiPage discussion.
makeProjectCommentActionPermissionsMap :: Maybe (Entity User) -> Text -> MakeActionPermissionsMap
makeProjectCommentActionPermissionsMap Nothing _ comments = makeLoggedOutCommentActionPermissionsMap comments
makeProjectCommentActionPermissionsMap (Just (Entity viewer_id viewer)) project_handle comments = do
        let map2 :: (a -> b) -> (a -> c) -> [a] -> ([b],[c])
            map2 f g = foldr (\a (bs, cs) -> (f a : bs, g a : cs)) ([],[])

            (comment_ids, user_ids) = map2 entityKey (commentUser . entityVal) comments

        (viewer_is_mod, user_map, closure_map, flag_map, comments_with_children) <- runYDB $ do
            Entity project_id _ <- getBy404 (UniqueProjectHandle project_handle)

            (,,,,) <$> userIsProjectModeratorDB viewer_id project_id
                   <*> (entitiesMap <$> fetchUsersInDB user_ids)
                   <*> makeClosureMapDB comment_ids
                   <*> makeFlagMapDB comment_ids
                   <*> (S.fromList <$> fetchCommentsWithChildrenInDB comment_ids)

        let viewer_is_established = userIsEstablished viewer
            viewer_can_close = userCanCloseComment viewer

            step :: Entity Comment
                 -> Map CommentId CommentActionPermissions
                 -> Map CommentId CommentActionPermissions
            step (Entity comment_id comment) =
                let user_id = commentUser comment
                    user = lookupErr "makeProjectCommentActionPermissions: user id not found in map" user_id user_map
                in M.insert comment_id (CommentActionPermissions
                       { can_add_tag   = viewer_is_established
                       , can_approve   = viewer_is_mod && not (commentIsApproved comment)
                       , can_close     = viewer_can_close && M.notMember comment_id closure_map
                       , can_delete    = viewer_id == user_id && S.notMember comment_id comments_with_children
                       , can_edit      = userCanEditComment viewer_id comment
                       , can_establish = viewer_is_mod && userIsUnestablished user
                       , can_flag      = viewer_is_established && viewer_id /= user_id && M.notMember comment_id flag_map
                       , can_reply     = True
                       , can_rethread  = viewer_is_mod || viewer_id == user_id
                       , can_retract   = viewer_id == user_id
                       })

        return (foldr step mempty comments)
