module Handler.Invitation where

import Import

import Model.Role

import Widgets.Sidebar


getInvitationR :: Text -> Handler RepHtml
getInvitationR code = do
    Entity invite_id invite <- runDB $ getBy404 $ UniqueInvite code
    maybe_user_id <- maybeAuthId

    when (maybe_user_id == Nothing)
        setUltDestCurrent

    alreadyExpired

    let redeemed = inviteRedeemed invite || inviteRedeemedBy invite /= Nothing

    defaultLayout $ $(widgetFile "invitation")
    

postInvitationR :: Text -> Handler RepHtml
postInvitationR code = do
    viewer_id <- requireAuthId
    now <- liftIO getCurrentTime
    role <- runDB $ do
        Entity invite_id invite <- getBy404 $ UniqueInvite code

        if inviteRedeemed invite
         then return Nothing
         else do
            update invite_id [ InviteRedeemed =. True
                             , InviteRedeemedTs =. Just now
                             , InviteRedeemedBy =. Just viewer_id
                             ]
            update viewer_id [ UserRole =. inviteRole invite ]
            return $ Just $ inviteRole invite

    redirect $ maybe (InvitationR code) roleDefaultTarget role
    
    
