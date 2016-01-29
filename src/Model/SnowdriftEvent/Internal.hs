module Model.SnowdriftEvent.Internal
    ( SnowdriftEvent(..)
    ) where

import Model

import Data.Either (Either)
import Data.Int (Int64)
import Data.Time (UTCTime)

-- A sum type of all events, each of which have their own database table.
data SnowdriftEvent
    -- Comment posted, whether it was approved immediately
    -- (because the poster is established), or approved by a moderator later.
    -- In the latter case, an ECommentApproved event *also* fires.
    = ECommentPosted CommentId Comment

    -- Comment posted by an unestablished user (pending approval).
    | ECommentPending CommentId Comment

    -- Comment approved by a moderator.
    | ECommentApproved CommentId Comment

    -- Comment closed (ticket or otherwise)
    | ECommentClosed CommentClosingId CommentClosing

    -- Ticket claimed
    | ETicketClaimed
        (Either
            (TicketClaimingId, TicketClaiming)
            (TicketOldClaimingId, TicketOldClaiming))

    -- Ticket unclaimed
    | ETicketUnclaimed TicketOldClaimingId TicketOldClaiming

    -- Comment rethreaded.
    | ECommentRethreaded RethreadId Rethread -- rethreaded-from-URL

    | EUserNotificationSent UserNotificationId UserNotification

    | EProjectNotificationSent ProjectNotificationId ProjectNotification

    -- New WikiEdit made.
    | EWikiEdit WikiEditId WikiEdit WikiTarget

    -- New WikiPage posted.
    | EWikiPage WikiPageId WikiPage WikiTarget

    -- New blog post posted.
    | EBlogPost BlogPostId BlogPost

    -- New pledge.
    | ENewPledge SharesPledgedId SharesPledged

    -- Pledge that has changed in value.
    | EUpdatedPledge Int64                         -- old shares
                     SharesPledgedId SharesPledged -- new pledge info

    -- Deleted pledge.
    | EDeletedPledge UTCTime UserId ProjectId Int64

    -- Volunteer application submitted.
    | EVolunteerApp UTCTime UserId ProjectId VolunteerApplicationId
