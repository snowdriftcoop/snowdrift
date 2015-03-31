module Model.User.Internal where

import Prelude
import Import hiding (UserNotificationPref)
import Model.Notification
    ( NotificationType (..), NotificationDelivery (..)
    , sendNotificationDB_, sendNotificationEmailDB )

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
      notifBalanceLow        :: NotificationDelivery
    , notifUnapprovedComment :: Maybe NotificationDelivery
    , notifRethreadedComment :: Maybe NotificationDelivery
    , notifReply             :: Maybe NotificationDelivery
    , notifEditConflict      :: NotificationDelivery
    , notifFlag              :: NotificationDelivery
    , notifFlagRepost        :: Maybe NotificationDelivery
    } deriving Show

userNotificationPref
    :: UserNotificationPref
    -> [(NotificationType, Maybe NotificationDelivery)]
userNotificationPref UserNotificationPref {..} =
    [ (NotifBalanceLow        , Just notifBalanceLow)
    , (NotifUnapprovedComment , notifUnapprovedComment)
    , (NotifRethreadedComment , notifRethreadedComment)
    , (NotifReply             , notifReply)
    , (NotifEditConflict      , Just notifEditConflict)
    , (NotifFlag              , Just notifFlag)
    , (NotifFlagRepost        , notifFlagRepost) ]

data ProjectNotificationPref = ProjectNotificationPref
    { notifWikiPage        :: Maybe NotificationDelivery
    , notifWikiEdit        :: Maybe NotificationDelivery
    , notifBlogPost        :: Maybe NotificationDelivery
    , notifNewPledge       :: Maybe NotificationDelivery
    , notifUpdatedPledge   :: Maybe NotificationDelivery
    , notifDeletedPledge   :: Maybe NotificationDelivery
    } deriving Show

projectNotificationPref
    :: ProjectNotificationPref
    -> [(NotificationType, Maybe NotificationDelivery)]
projectNotificationPref ProjectNotificationPref {..} =
    [ (NotifWikiEdit        , notifWikiEdit)
    , (NotifWikiPage        , notifWikiPage)
    , (NotifBlogPost        , notifBlogPost)
    , (NotifNewPledge       , notifNewPledge)
    , (NotifUpdatedPledge   , notifUpdatedPledge)
    , (NotifDeletedPledge   , notifDeletedPledge) ]


forcedNotification :: NotificationType -> Maybe NotificationDelivery
forcedNotification NotifWelcome             = Just NotifDeliverWebsite
forcedNotification NotifEligEstablish       = Just NotifDeliverWebsiteAndEmail
forcedNotification NotifBalanceLow          = Nothing
forcedNotification NotifUnapprovedComment   = Nothing
forcedNotification NotifApprovedComment     = Nothing
forcedNotification NotifRethreadedComment   = Nothing
forcedNotification NotifReply               = Nothing
forcedNotification NotifEditConflict        = Nothing
forcedNotification NotifFlag                = Nothing
forcedNotification NotifFlagRepost          = Nothing
forcedNotification NotifWikiEdit            = Nothing
forcedNotification NotifWikiPage            = Nothing
forcedNotification NotifBlogPost            = Nothing
forcedNotification NotifUpdatedPledge       = Nothing
forcedNotification NotifDeletedPledge       = Nothing
forcedNotification NotifNewPledge           = Nothing


-- | How does this User prefer notifications of a certain type to be delivered?
fetchUserNotificationPrefDB :: UserId -> Maybe ProjectId -> NotificationType
                            -> DB (Maybe NotificationDelivery)
fetchUserNotificationPrefDB user_id mproject_id notif_type = do
    -- Returned values should be unique, so it's okay to use
    -- 'listToMaybe' here, which keeps only the head of a list if it's
    -- not empty.
    pulled <- fmap (listToMaybe . unwrapValues) $ select $ from $ \ unp -> do
        where_ $ unp ^. UserNotificationPrefUser ==. val user_id
             &&. unp ^. UserNotificationPrefProject `notDistinctFrom` val mproject_id
             &&. unp ^. UserNotificationPrefType ==. val notif_type
        return $ unp ^. UserNotificationPrefDelivery
    -- 'mplus' throws away the second element if there are two
    -- 'Just's, but it shouldn't be an issue since forced
    -- notifications are not stored in the DB.
    return $ mplus forced pulled
  where
    forced = forcedNotification notif_type


fetchUsersByNotifPrefDB :: NotificationType -> Maybe ProjectId -> DB [UserId]
fetchUsersByNotifPrefDB notif_type mproject_id =
    fmap unwrapValues $
    -- A user may select multiple delivery methods, so this query will
    -- return duplicates without 'distinct'.
    selectDistinct $ from $ \ unp -> do
        where_ $ unp ^. UserNotificationPrefType ==. val notif_type
             &&. unp ^. UserNotificationPrefProject `notDistinctFrom` val mproject_id
        return $ unp ^. UserNotificationPrefUser

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

-- | Perform an action (or actions) according to the selected
-- 'NotificationDelivery' method.
sendPreferredNotificationDB :: Maybe NotificationSender -> NotificationReceiver
                            -> NotificationType -> Maybe ProjectId
                            -> Maybe CommentId-> Markdown -> SDB ()
sendPreferredNotificationDB mnotif_sender (NotificationReceiver notif_receiver)
                            notif_type mproject_id mcomment_id content =
    when (notificationSender `fmap` mnotif_sender /= Just notif_receiver) $ do
        mpref <- lift $
            fetchUserNotificationPrefDB notif_receiver mproject_id notif_type

        F.forM_ mpref $ \pref -> do
            muser_email <- lift $ fetchUserEmailVerified notif_receiver
            let sendEmailNotif   =
                    lift $ sendNotificationEmailDB
                               notif_type notif_receiver mproject_id content
                sendWebsiteNotif = do
                    r <- lift $ selectCount $ from $ \n -> do
                             where_ $ n ^. NotificationType
                                       ==. val notif_type
                                  &&. n ^. NotificationTo
                                       ==. val notif_receiver
                                  &&. n ^. NotificationProject
                                      `notDistinctFrom` val mproject_id
                                  &&. n ^. NotificationContent
                                       ==. val content
                    when (r == 0) $
                        sendNotificationDB_
                            notif_type notif_receiver mproject_id
                            mcomment_id content
            -- XXX: Support 'NotifDeliverEmailDigest'.
            case (pref, muser_email) of
                (NotifDeliverWebsiteAndEmail, Just _)  -> sendWebsiteNotif >> sendEmailNotif
                (NotifDeliverWebsiteAndEmail, Nothing) -> sendWebsiteNotif

                (NotifDeliverEmail          , Just _)  -> sendEmailNotif
                (NotifDeliverEmail          , Nothing) -> sendWebsiteNotif

                (NotifDeliverWebsite        , Just _)  -> sendWebsiteNotif
                (NotifDeliverWebsite        , Nothing) -> sendWebsiteNotif
