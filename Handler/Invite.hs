module Handler.Invite where

import Import

import System.Random
import Text.Printf
import Data.Text (pack, unpack)
import qualified Data.Map as M
import qualified Data.Set as S

import Control.Arrow

import Model.Role
import Model.User

import Widgets.Sidebar


inviteForm :: Role -> Form (Text, Role)
inviteForm role = renderBootstrap $ (,)
    <$> areq textField "About this invitation:" Nothing
    <*> areq (roleField role) "Type of Invite:" (Just GeneralPublic)

getInviteR :: Text -> Handler Html
getInviteR project_handle = do
    Entity viewer_id viewer <- requireAuth
    [ Value project_id ] <- runDB $ select $ from $ \ project -> do
        where_ $ project ^. ProjectHandle ==. val project_handle
        return $ project ^. ProjectId

    [ Value role ] <- runDB $ select $ from $ \ project_user_role -> do
        where_ $ project_user_role ^. UserProjectRoleUser ==. val viewer_id
                &&.  project_user_role ^. UserProjectRoleProject ==. val project_id
        return $ project_user_role ^. UserProjectRoleRole

    now <- liftIO getCurrentTime
    maybe_invite_code <- lookupSession "InviteCode"
    maybe_invite_role <- fmap (read . unpack) <$> lookupSession "InviteRole"
    deleteSession "InviteCode"
    deleteSession "InviteRole"
    let maybe_link = InvitationR project_handle <$> maybe_invite_code
    (invite_form, _) <- generateFormPost $ inviteForm role

    let can_view_all =
            case role of
                CommitteeMember -> True
                Admin -> True
                _ -> False

        restrict_view = 
                 if can_view_all
                    then const id
                    else (\ invite -> ((invite ^. InviteUser ==. val viewer_id) ||.))

    outstanding_invites <- runDB $ select $ from $ \ invite -> do
            where_ ( restrict_view invite $ invite ^. InviteRedeemed ==. val False )
            orderBy [ desc (invite ^. InviteCreatedTs) ]
            return invite

    redeemed_invites <- runDB $ select $ from $ \ invite -> do
            where_ ( restrict_view invite $ invite ^. InviteRedeemed ==. val True )
            orderBy [ desc (invite ^. InviteCreatedTs) ]
            limit 20
            return invite

    let redeemed_users = S.fromList $ mapMaybe (inviteRedeemedBy . entityVal) redeemed_invites
        redeemed_inviters = S.fromList $ map (inviteUser . entityVal) redeemed_invites
        outstanding_inviters = S.fromList $ map (inviteUser . entityVal) outstanding_invites
        user_ids = S.toList $ redeemed_users `S.union` redeemed_inviters `S.union` outstanding_inviters

    user_entities <- runDB $ selectList [ UserId <-. user_ids ] []

    let users = M.fromList $ map (entityKey &&& id) user_entities

    let format_user Nothing = "NULL"
        format_user (Just user_id) =
            let Entity _ user = users M.! user_id
             in fromMaybe (userIdent user) $ userName user

        format_inviter user_id =
            userPrintName $ users M.! user_id

    defaultLayout $(widgetFile "invite")


postInviteR :: Text -> Handler Html
postInviteR project_handle = do
    user_id <- requireAuthId
    now <- liftIO getCurrentTime
    invite <- liftIO randomIO
    [ (Value project_id, Value user_role) ] :: [(Value ProjectId, Value Role)] <- runDB $ select $ from $ \ (project `InnerJoin` project_user_role) -> do
        on $ project ^. ProjectId ==. project_user_role ^. UserProjectRoleProject
        where_ $ project ^. ProjectHandle ==. val project_handle
                &&. project_user_role ^. UserProjectRoleUser ==. val user_id
        return (project ^. ProjectId, project_user_role ^. UserProjectRoleRole)

    ((result, _), _) <- runFormPost $ inviteForm user_role
    case result of
        FormSuccess (tag, role) -> do
            let invite_code = pack $ printf "%016x" (invite :: Int64)
            _ <- runDB $ insert $ Invite now project_id invite_code user_id role tag False Nothing Nothing
            setSession "InviteCode" invite_code
            setSession "InviteRole" (pack $ show role)

        _ -> setMessage "Error in submitting form."

    redirect $ InviteR project_handle

