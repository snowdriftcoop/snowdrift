module Model.Notification.Internal where

import Prelude

import Database.Persist.TH
import Data.Text (Text)

data UserNotificationType
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
    deriving (Eq, Read, Show, Bounded, Enum)
derivePersistField "UserNotificationType"

data ProjectNotificationType
    -- New wiki page.
    = NotifWikiPage
    | NotifWikiEdit
    -- New blog post.
    | NotifBlogPost
    | NotifNewPledge
    | NotifUpdatedPledge
    | NotifDeletedPledge
    deriving (Eq, Read, Show, Bounded, Enum)
derivePersistField "ProjectNotificationType"

showUserNotificationType :: UserNotificationType -> Text
showUserNotificationType NotifWelcome           = "Snowdrift welcome message"
showUserNotificationType NotifEligEstablish     = "You have become eligible for establishment"
showUserNotificationType NotifUnapprovedComment = "Unapproved comments"
showUserNotificationType NotifApprovedComment   = "Approved comments"
showUserNotificationType NotifRethreadedComment = "Rethreaded comments"
showUserNotificationType NotifBalanceLow        = "Balance low"
showUserNotificationType NotifReply             = "Replies to my comments"
showUserNotificationType NotifEditConflict      = "Edit conflict"
showUserNotificationType NotifFlag              = "A comment of yours was flagged"
showUserNotificationType NotifFlagRepost        = "A comment you flagged was edited and reposted"

showProjectNotificationType :: ProjectNotificationType -> Text
showProjectNotificationType NotifWikiEdit          = "Wiki page was edited"
showProjectNotificationType NotifWikiPage          = "New wiki page"
showProjectNotificationType NotifBlogPost          = "New blog post"
showProjectNotificationType NotifNewPledge         = "New pledge"
showProjectNotificationType NotifUpdatedPledge     = "Pledge updated"
showProjectNotificationType NotifDeletedPledge     = "Pledge deleted"

data UserNotificationDelivery
    = UserNotifDeliverWebsite
    | UserNotifDeliverEmail
    | UserNotifDeliverWebsiteAndEmail
    deriving (Read, Show, Eq)
derivePersistField "UserNotificationDelivery"
data ProjectNotificationDelivery
    = ProjectNotifDeliverWebsite
    | ProjectNotifDeliverEmail
    | ProjectNotifDeliverWebsiteAndEmail
    deriving (Read, Show, Eq)
derivePersistField "ProjectNotificationDelivery"

