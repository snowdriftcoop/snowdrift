module Model.User.Internal where

import Prelude
import Import hiding (UserNotificationPref)
import Model.Notification
    ( NotificationType (..), NotificationDelivery (..)
    , sendNotificationDB_, sendNotificationEmailDB )

import qualified Data.Foldable      as F
import qualified Data.List.NonEmpty as N
import Data.List.NonEmpty (NonEmpty)

import Control.Monad.Trans.Maybe

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
      notifBalanceLow        :: NonEmpty NotificationDelivery
    , notifUnapprovedComment :: Maybe (NonEmpty NotificationDelivery)
    , notifRethreadedComment :: Maybe (NonEmpty NotificationDelivery)
    , notifReply             :: Maybe (NonEmpty NotificationDelivery)
    , notifEditConflict      :: NonEmpty NotificationDelivery
    , notifFlag              :: NonEmpty NotificationDelivery
    , notifFlagRepost        :: Maybe (NonEmpty NotificationDelivery)
    } deriving Show

userNotificationPref
    :: UserNotificationPref
    -> [(NotificationType, Maybe (NonEmpty NotificationDelivery))]
userNotificationPref UserNotificationPref {..} =
    [ (NotifBalanceLow        , Just notifBalanceLow)
    , (NotifUnapprovedComment , notifUnapprovedComment)
    , (NotifRethreadedComment , notifRethreadedComment)
    , (NotifReply             , notifReply)
    , (NotifEditConflict      , Just notifEditConflict)
    , (NotifFlag              , Just notifFlag)
    , (NotifFlagRepost        , notifFlagRepost) ]

data ProjectNotificationPref = ProjectNotificationPref
    { notifWikiPage        :: Maybe (NonEmpty NotificationDelivery)
    , notifWikiEdit        :: Maybe (NonEmpty NotificationDelivery)
    , notifBlogPost        :: Maybe (NonEmpty NotificationDelivery)
    , notifNewPledge       :: Maybe (NonEmpty NotificationDelivery)
    , notifUpdatedPledge   :: Maybe (NonEmpty NotificationDelivery)
    , notifDeletedPledge   :: Maybe (NonEmpty NotificationDelivery)
    } deriving Show

projectNotificationPref
    :: ProjectNotificationPref
    -> [(NotificationType, Maybe (NonEmpty NotificationDelivery))]
projectNotificationPref ProjectNotificationPref {..} =
    [ (NotifWikiEdit        , notifWikiEdit)
    , (NotifWikiPage        , notifWikiPage)
    , (NotifBlogPost        , notifBlogPost)
    , (NotifNewPledge       , notifNewPledge)
    , (NotifUpdatedPledge   , notifUpdatedPledge)
    , (NotifDeletedPledge   , notifDeletedPledge) ]


forcedNotification :: NotificationType -> Maybe (NonEmpty NotificationDelivery)
forcedNotification NotifWelcome             = Just $ return NotifDeliverWebsite
forcedNotification NotifEligEstablish       = Just $ N.fromList [ NotifDeliverWebsite, NotifDeliverEmail ]
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
                            -> DB (Maybe (NonEmpty NotificationDelivery))
fetchUserNotificationPrefDB user_id mproject_id notif_type = runMaybeT $ mplus forced pulled
  where
    forced = MaybeT $ return $ forcedNotification notif_type

    pulled = MaybeT $ fmap (N.nonEmpty . unwrapValues) $ select $ from $ \ unp -> do
        where_ $ unp ^. UserNotificationPrefUser ==. val user_id
            &&. unp ^. UserNotificationPrefProject `notDistinctFrom` val mproject_id
            &&. unp ^. UserNotificationPrefType ==. val notif_type
        return $ unp ^. UserNotificationPrefDelivery

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
        mprefs <- lift $
            fetchUserNotificationPrefDB notif_receiver mproject_id notif_type

        F.forM_ mprefs $ \ prefs -> F.forM_ prefs $ \ pref -> do
            muser_email <- lift $ fetchUserEmailVerified notif_receiver
            -- XXX: Support 'NotifDeliverEmailDigest'.
            if | pref == NotifDeliverEmail && isJust muser_email ->
                    lift $ sendNotificationEmailDB
                               notif_type notif_receiver mproject_id content
               | otherwise -> do
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
