module Handler.Invite where

import Import

import System.Random
import Text.Printf
import Data.Text (pack, unpack)
import qualified Data.Map as M
import qualified Data.Set as S

import Model.Role
import Model.User

import Widgets.Sidebar


inviteForm :: Form (Text, Role)
inviteForm = renderBootstrap $ (,)
    <$> areq textField "About this invitation:" Nothing
    <*> areq roleField "Type of Invite:" (Just TeamMember)

getInviteR :: Text -> Handler Html
getInviteR project_handle = do
    Entity viewer_id viewer <- requireAuth
    admin <- runDB $ (||)
        <$> isProjectAdmin project_handle viewer_id
        <*> isProjectAdmin "snowdrift" viewer_id

    when (not admin) $ permissionDenied "must be an admin to invite"

    Entity project_id project <- runDB $ getBy404 $ UniqueProjectHandle project_handle

    now <- liftIO getCurrentTime
    maybe_invite_code <- lookupSession "InviteCode"
    maybe_invite_role <- fmap (read . unpack) <$> lookupSession "InviteRole"
    deleteSession "InviteCode"
    deleteSession "InviteRole"
    let maybe_link = InvitationR project_handle <$> maybe_invite_code
    (invite_form, _) <- generateFormPost inviteForm

    outstanding_invites <- runDB $ select $ from $ \ invite -> do
            where_ ( invite ^. InviteRedeemed ==. val False )
            orderBy [ desc (invite ^. InviteCreatedTs) ]
            return invite

    redeemed_invites <- runDB $ select $ from $ \ invite -> do
            where_ ( invite ^. InviteRedeemed ==. val True )
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

    admin <- runDB $ (||)
        <$> isProjectAdmin project_handle user_id
        <*> isProjectAdmin "snowdrift" user_id

    when (not admin) $ permissionDenied "must be an admin to invite"

    now <- liftIO getCurrentTime
    invite <- liftIO randomIO
    project_id <- fmap entityKey $ runDB $ getBy404 $ UniqueProjectHandle project_handle

    ((result, _), _) <- runFormPost inviteForm
    case result of
        FormSuccess (tag, role) -> do
            let invite_code = pack $ printf "%016x" (invite :: Int64)
            _ <- runDB $ insert $ Invite now project_id invite_code user_id role tag False Nothing Nothing
            setSession "InviteCode" invite_code
            setSession "InviteRole" (pack $ show role)

        _ -> setMessage "Error in submitting form."

    redirect $ InviteR project_handle

