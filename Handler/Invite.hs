module Handler.Invite where

import Import

import System.Random
import Text.Printf
import Data.Text (pack, unpack)
import qualified Data.Map as M
import Data.Maybe (fromJust)

import Model.Role

import Widgets.Sidebar


inviteForm :: Role -> Form (Text, Role)
inviteForm role = renderBootstrap $ (,)
    <$> areq textField "About this invitation:" Nothing
    <*> areq (roleField role) "Type of Invite:" (Just GeneralPublic)

getInviteR :: Handler RepHtml
getInviteR = do
    Entity viewer_id viewer <- requireAuth
    now <- liftIO getCurrentTime
    maybe_invite_code <- lookupSession "InviteCode"
    maybe_invite_role <- fmap (read . unpack) <$> lookupSession "InviteRole"
    deleteSession "InviteCode"
    deleteSession "InviteRole"
    let maybe_link = InvitationR <$> maybe_invite_code
    (invite_form, _) <- generateFormPost $ inviteForm (userRole viewer)

    let outstanding_invite_filter =
            (case userRole viewer of
                CommitteeMember -> []
                Admin -> []
                _ -> [ InviteUser ==. viewer_id ]
            ) ++ [ InviteRedeemed ==. False ]
        redeemed_invite_filter =
            (case userRole viewer of
                CommitteeMember -> []
                Admin -> []
                _ -> [ InviteUser ==. viewer_id ]
            ) ++ [ InviteRedeemed ==. True ]

    outstanding_invites <- runDB $ selectList outstanding_invite_filter [ Desc InviteCreatedTs ]
    redeemed_invites <- runDB $ selectList redeemed_invite_filter [ Desc InviteRedeemedTs, LimitTo 20 ]
    redeemed_users <- runDB $ selectList [ UserId <-. map (fromJust . inviteRedeemedBy . entityVal) redeemed_invites ] []

    let redeemed_users_map = M.fromList $ map (\ (Entity a b) -> (a, b)) redeemed_users

    let format_user Nothing = "NULL"
        format_user (Just user_id) =
            let user = redeemed_users_map M.! user_id
             in fromMaybe (userIdent user) $ userName user

    defaultLayout $(widgetFile "invite")


postInviteR :: Handler RepHtml
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

