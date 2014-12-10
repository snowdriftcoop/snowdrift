module Model.User.Internal where

import Prelude
import Import hiding (UserNotificationPref)
import Model.Notification
    ( NotificationType (..), NotificationDelivery (..)
    , sendNotificationDB_, sendNotificationEmailDB )

import qualified Data.Foldable      as F
import qualified Data.List.NonEmpty as N
import Data.List.NonEmpty (NonEmpty)

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
    { notifTicketClaimed   :: Maybe (NonEmpty NotificationDelivery)
    , notifTicketUnclaimed :: Maybe (NonEmpty NotificationDelivery)
    , notifWikiEdit        :: Maybe (NonEmpty NotificationDelivery)
    , notifWikiPage        :: Maybe (NonEmpty NotificationDelivery)
    , notifBlogPost        :: Maybe (NonEmpty NotificationDelivery)
    , notifNewPledge       :: Maybe (NonEmpty NotificationDelivery)
    , notifUpdatedPledge   :: Maybe (NonEmpty NotificationDelivery)
    , notifDeletedPledge   :: Maybe (NonEmpty NotificationDelivery)
    } deriving Show

projectNotificationPref
    :: ProjectNotificationPref
    -> [(NotificationType, Maybe (NonEmpty NotificationDelivery))]
projectNotificationPref ProjectNotificationPref {..} =
    [ (NotifTicketClaimed   , notifTicketClaimed)
    , (NotifTicketUnclaimed , notifTicketUnclaimed)
    , (NotifWikiEdit        , notifWikiEdit)
    , (NotifWikiPage        , notifWikiPage)
    , (NotifBlogPost        , notifBlogPost)
    , (NotifNewPledge       , notifNewPledge)
    , (NotifUpdatedPledge   , notifUpdatedPledge)
    , (NotifDeletedPledge   , notifDeletedPledge) ]

-- | How does this User prefer notifications of a certain type to be delivered?
fetchUserNotificationPrefDB :: UserId -> Maybe ProjectId -> NotificationType
                            -> DB (Maybe (NonEmpty NotificationDelivery))
fetchUserNotificationPrefDB user_id mproject_id notif_type
    = (\case [] -> Nothing
             xs -> Just $ unValue <$> N.fromList xs)
  <$> (select $
       from $ \unp -> do
       where_ $ unp ^. UserNotificationPrefUser ==. val user_id
            &&. unp ^. UserNotificationPrefProject `notDistinctFrom` val mproject_id
            &&. unp ^. UserNotificationPrefType ==. val notif_type
       return (unp ^. UserNotificationPrefDelivery))

fetchUsersByNotifPrefDB :: NotificationType -> Maybe ProjectId -> DB [UserId]
fetchUsersByNotifPrefDB notif_type mproject_id =
    fmap unwrapValues $
    -- A user may select multiple delivery methods, so this query will
    -- return duplicates without 'distinct'.
    selectDistinct $ from $ \unp -> do
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

fetchUserEmailVerified :: UserId -> DB Bool
fetchUserEmailVerified user_id =
    fmap (\[Value b] -> b) $
    select $ from $ \user -> do
        where_ $ user ^. UserId ==. val user_id
        return $ user ^. UserEmail_verified

-- | Perform an action (or actions) according to the selected
-- 'NotificationDelivery' method.
sendPreferredNotificationDB :: UserId -> NotificationType -> Maybe ProjectId
                            -> Maybe CommentId-> Markdown -> SDB ()
sendPreferredNotificationDB user_id notif_type mproject_id mcomment_id content = do
    mprefs <- lift $ fetchUserNotificationPrefDB user_id mproject_id notif_type
    F.forM_ mprefs $ \prefs -> F.forM_ prefs $ \pref -> do
        muser_email    <- lift $ fetchUserEmail user_id
        email_verified <- lift $ fetchUserEmailVerified user_id
        -- XXX: Support 'NotifDeliverEmailDigest'.
        if | pref == NotifDeliverEmail && isJust muser_email && email_verified ->
                lift $ sendNotificationEmailDB notif_type user_id mproject_id content
           | otherwise -> do
                 r <- selectCount $ from $ \n -> do
                          where_ $ n ^. NotificationType    ==. val notif_type
                               &&. n ^. NotificationTo      ==. val user_id
                               &&. n ^. NotificationProject `notDistinctFrom` val mproject_id
                               &&. n ^. NotificationContent ==. val content
                 when (r == 0) $
                     sendNotificationDB_ notif_type user_id mproject_id mcomment_id content
