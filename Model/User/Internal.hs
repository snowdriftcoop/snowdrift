module Model.User.Internal where

import Prelude

import Model.Notification.Internal

import Data.Text      (Text)
import Yesod.Markdown (Markdown)

data UserUpdate =
    UserUpdate
        { userUpdateName               :: Maybe Text
        , userUpdateAvatar             :: Maybe Text
        , userUpdateIrcNick            :: Maybe Text
        , userUpdateBlurb              :: Maybe Markdown
        , userUpdateStatement          :: Maybe Markdown
--      , userUpdateNotificationPreferences :: [(NotificationType, NotificationDelivery)]
        }
