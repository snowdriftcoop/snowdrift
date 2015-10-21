{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module PPrint where

import Prelude

import Database.Persist.Sql
import Model
import Model.Notification
import Data.Text (Text)
import qualified Data.Text as T


class Show a => PPrint a where
    pprint :: a -> String

instance PPrint CommentId where
    pprint = show . unSqlBackendKey . unCommentKey

instance PPrint UserId where
    pprint = show . unSqlBackendKey . unUserKey

instance PPrint ProjectId where
    pprint = show . unSqlBackendKey . unProjectKey

instance PPrint TagId where
    pprint = show . unSqlBackendKey . unTagKey

instance PPrint ProjectNotificationType where
    pprint = show

instance PPrint ProjectNotificationDelivery where
    pprint = show

instance PPrint Text where
    pprint = T.unpack
