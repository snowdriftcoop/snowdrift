module Model.User.Internal where

import Prelude
import Import
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

data NotificationPref = NotificationPref
    { -- 'NotifWelcome' and 'NotifEligEstablish' are not handled since
      -- they are delivered only once.
      notifBalanceLow        :: NonEmpty NotificationDelivery
    , notifUnapprovedComment :: NonEmpty NotificationDelivery
    , notifRethreadedComment :: Maybe (NonEmpty NotificationDelivery)
    , notifReply             :: Maybe (NonEmpty NotificationDelivery)
    , notifEditConflict      :: NonEmpty NotificationDelivery
    , notifFlag              :: NonEmpty NotificationDelivery
    , notifFlagRepost        :: Maybe (NonEmpty NotificationDelivery)
    } deriving Show

-- | How does this User prefer notifications of a certain type to be delivered?
fetchUserNotificationPrefDB :: UserId -> NotificationType -> DB (Maybe (NonEmpty NotificationDelivery))
fetchUserNotificationPrefDB user_id notif_type
    = (\case [] -> Nothing
             xs -> Just $ unValue <$> N.fromList xs)
  <$> (select $
       from $ \unp -> do
       where_ $
           unp ^. UserNotificationPrefUser ==. val user_id &&.
           unp ^. UserNotificationPrefType ==. val notif_type
       return (unp ^. UserNotificationPrefDelivery))

fetchUserEmail :: UserId -> DB (Maybe Text)
fetchUserEmail user_id
    = (\case []    -> Nothing
             (x:_) -> unValue x)
  <$> (select $ from $ \user -> do
           where_ $ user ^. UserId ==. val user_id
           return $ user ^. UserEmail)

-- | Perform an action (or actions) according to the selected
-- 'NotificationDelivery' method.
sendPreferredNotificationDB :: UserId -> NotificationType -> Maybe ProjectId
                            -> Maybe CommentId-> Markdown -> SDB ()
sendPreferredNotificationDB user_id notif_type mproject_id mcomment_id content = do
    mprefs <- lift $ fetchUserNotificationPrefDB user_id notif_type
    F.forM_ mprefs $ \prefs -> F.forM_ prefs $ \pref -> do
        muser_email <- lift $ fetchUserEmail user_id
        -- XXX: Support 'NotifDeliverEmailDigest'.
        if | pref == NotifDeliverEmail && isJust muser_email ->
             lift $ sendNotificationEmailDB notif_type user_id mproject_id content
           | otherwise ->
             sendNotificationDB_ notif_type user_id mproject_id mcomment_id content
