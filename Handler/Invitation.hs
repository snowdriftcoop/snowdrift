module Handler.Invitation where

import Import

import Model.Role

import Widgets.Sidebar


getInvitationR :: Text -> Handler Html
getInvitationR code = do
    Entity invite_id invite <- runDB $ getBy404 $ UniqueInvite code
    maybe_user_id <- maybeAuthId

    when (maybe_user_id == Nothing)
        setUltDestCurrent

    alreadyExpired

    let redeemed = inviteRedeemed invite || isJust (inviteRedeemedBy invite)

    defaultLayout $ $(widgetFile "invitation")
    

postInvitationR :: Text -> Handler Html
postInvitationR code = do
    viewer_id :: UserId <- requireAuthId
    now <- liftIO getCurrentTime
    role <- runDB $ do
        Entity invite_id invite <- getBy404 $ UniqueInvite code

        if inviteRedeemed invite
         then return Nothing
         else do
            update $ \ i -> do
                set i [ InviteRedeemed =. val True
                      , InviteRedeemedTs =. val (Just now)
                      , InviteRedeemedBy =. val (Just viewer_id)
                      ]
                where_ ( i ^. InviteId ==. val invite_id )

            update $ \ user -> do
                where_ (user ^. UserId ==. val viewer_id)
                set user [ UserRole =. val (inviteRole invite) ]

            return $ Just $ inviteRole invite

    redirect $ maybe (InvitationR code) roleDefaultTarget role
    
    
