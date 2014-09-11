module Model.Notification.Internal where

import Prelude

import Database.Persist.TH
import Data.Text (Text)

data NotificationType
    = NotifWelcome
    -- User has become eligible for establishment.
    | NotifEligEstablish
    -- Balance low (can't be ignored)
    | NotifBalanceLow
    -- Alerts about unapproved comments. These are delivered to
    -- the poster (to inform him/her about establishment, etc) and
    -- to every moderator of the Project that the Comment was posted
    -- on. The notifications sent to moderators are automatically
    -- deleted when the comment is approved.
    | NotifUnapprovedComment
    -- User's comment was approved.
    | NotifApprovedComment
    -- User's comment was rethreaded.
    | NotifRethreadedComment
    -- Reply to a comment made.
    | NotifReply
    -- Edit conflict.
    | NotifEditConflict
    -- Comment flagged.
    | NotifFlag
    -- Flagged comment was reposted.
    | NotifFlagRepost
    deriving (Eq, Read, Show)
derivePersistField "NotificationType"

showNotificationType :: NotificationType -> Text
showNotificationType NotifWelcome           = "Snowdrift welcome message"
showNotificationType NotifEligEstablish     = "You have become eligible for establishment"
showNotificationType NotifUnapprovedComment = "Unapproved comments"
showNotificationType NotifApprovedComment   = "Approved comments"
showNotificationType NotifRethreadedComment = "Rethreaded comments"
showNotificationType NotifBalanceLow        = "Balance low"
showNotificationType NotifReply             = "Replies to my comments"
showNotificationType NotifEditConflict      = "Edit conflict"
showNotificationType NotifFlag              = "A comment of yours was flagged"
showNotificationType NotifFlagRepost        = "A comment you flagged was edited and reposted"

data NotificationDelivery
    = NotifDeliverInternal     -- Only send notifications.
    | NotifDeliverEmail        -- Send email in addition to notifications.
    | NotifDeliverEmailDigest  -- Send email digest in addition to notifications (sent immediately).
    deriving (Read, Show)
derivePersistField "NotificationDelivery"
