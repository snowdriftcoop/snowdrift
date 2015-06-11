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
    -- New Volunteer Application Submission
    | NotifVolunteerApp
    deriving (Eq, Read, Show, Bounded, Enum)
derivePersistField "ProjectNotificationType"

showUserNotificationType :: UserNotificationType -> Text
showUserNotificationType = \case
    NotifWelcome           -> "Snowdrift welcome message"
    NotifEligEstablish     -> "You have become eligible for establishment"
    NotifUnapprovedComment -> "Unapproved comments"
    NotifApprovedComment   -> "Approved comments"
    NotifRethreadedComment -> "Rethreaded comments"
    NotifBalanceLow        -> "Balance low"
    NotifReply             -> "Replies to my comments"
    NotifEditConflict      -> "Edit conflict"
    NotifFlag              -> "A comment of yours was flagged"
    NotifFlagRepost        -> "A comment you flagged was edited and reposted"

showProjectNotificationType :: ProjectNotificationType -> Text
showProjectNotificationType NotifWikiEdit          = "Wiki page was edited"
showProjectNotificationType NotifWikiPage          = "New wiki page"
showProjectNotificationType NotifBlogPost          = "New blog post"
showProjectNotificationType NotifNewPledge         = "New pledge"
showProjectNotificationType NotifUpdatedPledge     = "Pledge updated"
showProjectNotificationType NotifDeletedPledge     = "Pledge deleted"
showProjectNotificationType NotifVolunteerApp      = "New Volunteer Application submitted"

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

