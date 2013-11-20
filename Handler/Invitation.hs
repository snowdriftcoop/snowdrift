module Handler.Invitation where

import Import

-- import Model.Role

import Widgets.Sidebar


getInvitationR :: Text -> Text -> Handler Html
getInvitationR project_handle code = do
    Entity invite_id invite <- runDB $ getBy404 $ UniqueInvite code
    maybe_user_id <- maybeAuthId

    when (maybe_user_id == Nothing)
        setUltDestCurrent

    alreadyExpired

    let redeemed = inviteRedeemed invite || isJust (inviteRedeemedBy invite)

    defaultLayout $(widgetFile "invitation")
    

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
    
    
