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

getInviteR :: Handler Html
getInviteR = do
    Entity viewer_id viewer <- requireAuth
    now <- liftIO getCurrentTime
    maybe_invite_code <- lookupSession "InviteCode"
    maybe_invite_role <- fmap (read . unpack) <$> lookupSession "InviteRole"
    deleteSession "InviteCode"
    deleteSession "InviteRole"
    let maybe_link = InvitationR <$> maybe_invite_code
    (invite_form, _) <- generateFormPost $ inviteForm (userRole viewer)

    let can_view_all =
            case userRole viewer of
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


postInviteR :: Handler Html
postInviteR = do
    Entity user_id user <- requireAuth
    now <- liftIO getCurrentTime
    invite <- liftIO randomIO
    ((result, _), _) <- runFormPost $ inviteForm (userRole user)
    case result of
        FormSuccess (tag, role) -> do
            let invite_code = pack $ printf "%016x" (invite :: Int64)
            _ <- runDB $ insert $ Invite now invite_code user_id role tag False Nothing Nothing
            setSession "InviteCode" invite_code
            setSession "InviteRole" (pack $ show role)

        _ -> setMessage "Error in submitting form."

    redirect InviteR

