module Handler.Invitation where

import Import

import Widgets.Sidebar


getOldInvitationR :: Text -> Handler RepHtml
getOldInvitationR = getInvitationR "old_snowdrift"

getInvitationR :: Text -> Text -> Handler RepHtml
getInvitationR project_handle code = do
    Entity project_id project <- runDB $ getBy404 $ UniqueProjectHandle project_handle
    Entity invite_id invite <- runDB $ getBy404 $ UniqueInvite project_id code

    maybe_user_id <- maybeAuthId

    when (isNothing maybe_user_id)
        setUltDestCurrent

    alreadyExpired

    let redeemed = inviteRedeemed invite || isJust (inviteRedeemedBy invite)

    defaultLayout $ $(widgetFile "invitation")
    

postOldInvitationR :: Text -> Handler RepHtml
postOldInvitationR = postInvitationR "old_snowdrift"

postInvitationR :: Text -> Text -> Handler RepHtml
postInvitationR project_handle code = do
    viewer_id <- requireAuthId

    Entity project_id _ <- runDB $ getBy404 $ UniqueProjectHandle project_handle

    now <- liftIO getCurrentTime
    runDB $ do
        Entity invite_id invite <- getBy404 $ UniqueInvite project_id code

        if inviteRedeemed invite
         then do
            lift $ setMessage "sorry, that invitation was redeemed earlier"
            lift $ redirect $ InvitationR project_handle code

         else do
            update invite_id [ InviteRedeemed =. True
                             , InviteRedeemedTs =. Just now
                             , InviteRedeemedBy =. Just viewer_id
                             ]

            void $ insert $ ProjectUserRole project_id viewer_id (inviteRole invite)


            project <- get404 project_id
            lift $ setMessage "invitation redeemed"
            lift $ redirect $ ProjectR (projectHandle project)
    
    
