module Model.User.Internal where

import Prelude
import Import hiding (UserNotificationPref, ProjectNotificationPref)
import Model.Notification
    ( UserNotificationType (..), UserNotificationDelivery (..)
    , sendUserNotificationDB_, sendUserNotificationEmailDB
    , ProjectNotificationType (..), ProjectNotificationDelivery (..)
    , sendProjectNotificationDB_, sendProjectNotificationEmailDB )

import qualified Data.Foldable      as F

data UserUpdate =
    UserUpdate
        { userUpdateName               :: Maybe Text
        , userUpdateAvatar             :: Maybe Text
        , userUpdateEmail              :: Maybe Text
        , userUpdateIrcNick            :: Maybe Text
        , userUpdateBlurb              :: Maybe Markdown
        , userUpdateStatement          :: Maybe Markdown
        }

data ChangePassword = ChangePassword
    { currentPassword :: Text
    , newPassword     :: Text
    , newPassword'    :: Text
    }

data SetPassword = SetPassword
    { password  :: Text
    , password' :: Text
    }

data UserNotificationPref = UserNotificationPref
    { -- 'NotifWelcome' and 'NotifEligEstablish' are not handled since
      -- they are delivered only once.
      notifBalanceLow        :: UserNotificationDelivery
    , notifUnapprovedComment :: Maybe UserNotificationDelivery
    , notifRethreadedComment :: Maybe UserNotificationDelivery
    , notifReply             :: Maybe UserNotificationDelivery
    , notifEditConflict      :: UserNotificationDelivery
    , notifFlag              :: UserNotificationDelivery
    , notifFlagRepost        :: Maybe UserNotificationDelivery
    } deriving Show

userNotificationPref
    :: UserNotificationPref
    -> [(UserNotificationType, Maybe UserNotificationDelivery)]
userNotificationPref UserNotificationPref {..} =
    [ (NotifBalanceLow        , Just notifBalanceLow)
    , (NotifUnapprovedComment , notifUnapprovedComment)
    , (NotifRethreadedComment , notifRethreadedComment)
    , (NotifReply             , notifReply)
    , (NotifEditConflict      , Just notifEditConflict)
    , (NotifFlag              , Just notifFlag)
    , (NotifFlagRepost        , notifFlagRepost) ]

data ProjectNotificationPref = ProjectNotificationPref
    { notifWikiPage        :: Maybe ProjectNotificationDelivery
    , notifWikiEdit        :: Maybe ProjectNotificationDelivery
    , notifBlogPost        :: Maybe ProjectNotificationDelivery
    , notifNewPledge       :: Maybe ProjectNotificationDelivery
    , notifUpdatedPledge   :: Maybe ProjectNotificationDelivery
    , notifDeletedPledge   :: Maybe ProjectNotificationDelivery
    } deriving Show

projectNotificationPref
    :: ProjectNotificationPref
    -> [(ProjectNotificationType, Maybe ProjectNotificationDelivery)]
projectNotificationPref ProjectNotificationPref {..} =
    [ (NotifWikiEdit        , notifWikiEdit)
    , (NotifWikiPage        , notifWikiPage)
    , (NotifBlogPost        , notifBlogPost)
    , (NotifNewPledge       , notifNewPledge)
    , (NotifUpdatedPledge   , notifUpdatedPledge)
    , (NotifDeletedPledge   , notifDeletedPledge) ]


forcedNotification :: UserNotificationType -> Maybe UserNotificationDelivery
forcedNotification NotifWelcome             = Just UserNotifDeliverWebsite
forcedNotification NotifEligEstablish       = Just UserNotifDeliverWebsiteAndEmail
forcedNotification NotifBalanceLow          = Nothing
forcedNotification NotifUnapprovedComment   = Nothing
forcedNotification NotifApprovedComment     = Nothing
forcedNotification NotifRethreadedComment   = Nothing
forcedNotification NotifReply               = Nothing
forcedNotification NotifEditConflict        = Nothing
forcedNotification NotifFlag                = Nothing
forcedNotification NotifFlagRepost          = Nothing

-- | How does this User prefer notifications of a certain type to be delivered?
fetchUserNotificationPrefDB :: UserId -> UserNotificationType
                            -> DB (Maybe UserNotificationDelivery)
fetchUserNotificationPrefDB user_id notif_type = do
    -- Returned values should be unique, so it's okay to use
    -- 'listToMaybe' here, which keeps only the head of a list if it's
    -- not empty.
    pulled <- fmap (listToMaybe . unwrapValues) $ select $ from $ \ unp -> do
        where_ $ unp ^. UserNotificationPrefUser ==. val user_id
             &&. unp ^. UserNotificationPrefType ==. val notif_type
        limit 1
        return $ unp ^. UserNotificationPrefDelivery
    -- 'mplus' throws away the second element if there are two
    -- 'Just's, but it shouldn't be an issue since forced
    -- notifications are not stored in the DB.
    return $ mplus forced pulled
  where
    forced = forcedNotification notif_type

fetchProjectNotificationPrefDB :: UserId -> ProjectId -> ProjectNotificationType
                               -> DB (Maybe ProjectNotificationDelivery)
fetchProjectNotificationPrefDB user_id project_id notif_type =
    -- Returned values should be unique, so it's okay to use
    -- 'listToMaybe' here, which keeps only the head of a list if it's
    -- not empty.
    fmap (listToMaybe . unwrapValues) $ select $ from $ \ pnp -> do
        where_ $ pnp ^. ProjectNotificationPrefUser    ==. val user_id
             &&. pnp ^. ProjectNotificationPrefProject ==. val project_id
             &&. pnp ^. ProjectNotificationPrefType    ==. val notif_type
        limit 1
        return $ pnp ^. ProjectNotificationPrefDelivery

fetchUsersByUserNotifPrefDB :: UserNotificationType -> DB [UserId]
fetchUsersByUserNotifPrefDB notif_type =
    fmap unwrapValues $
    select $ from $ \ unp -> do
        where_ $ unp ^. UserNotificationPrefType ==. val notif_type
        return $ unp ^. UserNotificationPrefUser

fetchUsersByProjectNotifPrefDB :: ProjectNotificationType -> ProjectId
                               -> DB [UserId]
fetchUsersByProjectNotifPrefDB notif_type project_id =
    fmap unwrapValues $
    select $ from $ \ pnp -> do
        where_ $ pnp ^. ProjectNotificationPrefType
             ==. val notif_type
             &&. pnp ^. ProjectNotificationPrefProject
             ==. val project_id
        return $ pnp ^. ProjectNotificationPrefUser

fetchUserEmail :: UserId -> DB (Maybe Text)
fetchUserEmail user_id
    = (\case []    -> Nothing
             (x:_) -> unValue x)
  <$> (select $ from $ \user -> do
           where_ $ user ^. UserId ==. val user_id
           return $ user ^. UserEmail)

fetchUserEmailVerified :: UserId -> DB (Maybe Text)
fetchUserEmailVerified user_id
    = (\case []    -> Nothing
             (x:_) -> unValue x)
  <$> (select $ from $ \user -> do
           where_ $ user ^. UserId ==. val user_id
                &&. user ^. UserEmail_verified
           return $ user ^. UserEmail)

-- Distinguish the user who receives a notification from the one who
-- triggers it, so the notification is not delivered when the two are
-- the same user.
data NotificationSender   = NotificationSender
    { notificationSender   :: UserId }
data NotificationReceiver = NotificationReceiver
    { notificationReceiver :: UserId }

-- | Send a 'UserNotification' according to the selected delivery
-- method.
sendPreferredUserNotificationDB :: Maybe NotificationSender
                                -> NotificationReceiver -> UserNotificationType
                                -> Maybe CommentId -> Markdown -> SDB ()
sendPreferredUserNotificationDB mnotif_sender
                                (NotificationReceiver notif_receiver)
                                notif_type mcomment_id content =
    when (notificationSender `fmap` mnotif_sender /= Just notif_receiver) $ do
        mpref <- lift $
            fetchUserNotificationPrefDB notif_receiver notif_type

        F.forM_ mpref $ \pref -> do
            muser_email <- lift $ fetchUserEmailVerified notif_receiver
            let sendEmailNotif   =
                    lift $ sendUserNotificationEmailDB
                               notif_type notif_receiver content
                sendWebsiteNotif = do
                    r <- lift $ selectCount $ from $ \n -> do
                             where_ $ n ^. UserNotificationType
                                       ==. val notif_type
                                  &&. n ^. UserNotificationTo
                                       ==. val notif_receiver
                                  &&. n ^. UserNotificationContent
                                       ==. val content
                    when (r == 0) $
                        sendUserNotificationDB_
                            notif_type notif_receiver mcomment_id content
            case (pref, muser_email) of
                (UserNotifDeliverWebsiteAndEmail, Just _)  -> sendWebsiteNotif >> sendEmailNotif
                (UserNotifDeliverWebsiteAndEmail, Nothing) -> sendWebsiteNotif

                (UserNotifDeliverEmail          , Just _)  -> sendEmailNotif
                (UserNotifDeliverEmail          , Nothing) -> sendWebsiteNotif

                (UserNotifDeliverWebsite        , Just _)  -> sendWebsiteNotif
                (UserNotifDeliverWebsite        , Nothing) -> sendWebsiteNotif

-- | Send a 'ProjectNotification' according to the selected delivery
-- method.
sendPreferredProjectNotificationDB :: Maybe NotificationSender
                                   -> NotificationReceiver
                                   -> ProjectNotificationType
                                   -> ProjectId -> Markdown -> SDB ()
sendPreferredProjectNotificationDB mnotif_sender
                                   (NotificationReceiver notif_receiver)
                                   notif_type project_id content =
    when (notificationSender `fmap` mnotif_sender /= Just notif_receiver) $ do
        mpref <- lift $
            fetchProjectNotificationPrefDB notif_receiver project_id notif_type

        F.forM_ mpref $ \pref -> do
            muser_email <- lift $ fetchUserEmailVerified notif_receiver
            let sendEmailNotif   =
                    lift $ sendProjectNotificationEmailDB
                               notif_type notif_receiver project_id content
                sendWebsiteNotif = do
                    r <- lift $ selectCount $ from $ \n -> do
                             where_ $ n ^. ProjectNotificationType
                                       ==. val notif_type
                                  &&. n ^. ProjectNotificationTo
                                       ==. val notif_receiver
                                  &&. n ^. ProjectNotificationProject
                                       ==. val project_id
                                  &&. n ^. ProjectNotificationContent
                                       ==. val content
                    when (r == 0) $
                        sendProjectNotificationDB_
                            notif_type notif_receiver project_id content
            case (pref, muser_email) of
                (ProjectNotifDeliverWebsiteAndEmail, Just _)  -> sendWebsiteNotif >> sendEmailNotif
                (ProjectNotifDeliverWebsiteAndEmail, Nothing) -> sendWebsiteNotif

                (ProjectNotifDeliverEmail          , Just _)  -> sendEmailNotif
                (ProjectNotifDeliverEmail          , Nothing) -> sendWebsiteNotif

                (ProjectNotifDeliverWebsite        , Just _)  -> sendWebsiteNotif
                (ProjectNotifDeliverWebsite        , Nothing) -> sendWebsiteNotif
