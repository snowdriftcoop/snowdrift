{- Each project can generate invitation codes to give users special roles such as Moderator or Team Member or Admin.
Invitation.hs is where invited users go to actually redeem invitations they receive. -}

module Handler.Invitation where

import Import
import qualified Data.Text as T
-- import Model.Role



getInvitationR :: Text -> Text -> Handler Html
getInvitationR project_handle code = do
    (Entity _ project, Entity _ invite) <- runDB $ (,)
        <$> getBy404 (UniqueProjectHandle project_handle)
        <*> getBy404 (UniqueInvite code)
    maybe_user_id    <- maybeAuthId

    unless (isJust maybe_user_id) setUltDestCurrent

    alreadyExpired

    let redeemed = inviteRedeemed invite || isJust (inviteRedeemedBy invite)

    defaultLayout $ do
        setTitle . toHtml $ projectName project <> " Invitation - " <> (T.pack . show $ inviteRole invite) <> " | Snowdrift.coop"
        $(widgetFile "invitation")


postInvitationR :: Text -> Text -> Handler Html
postInvitationR _ code = do
    viewer_id :: UserId <- requireAuthId
    now <- liftIO getCurrentTime
    _ <- runDB $ do
        Entity invite_id invite <- getBy404 $ UniqueInvite code

        if inviteRedeemed invite
         then return Nothing
         else do
            -- TODO make sure project handle matches invite
            update $ \ i -> do
                set i [ InviteRedeemed =. val True
                      , InviteRedeemedTs =. val (Just now)
                      , InviteRedeemedBy =. val (Just viewer_id)
                      ]
                where_ ( i ^. InviteId ==. val invite_id )

            _ <- insertUnique $ ProjectUserRole (inviteProject invite) viewer_id (inviteRole invite)
            -- TODO: update

            return $ Just $ inviteRole invite

    redirectUltDest HomeR


