module Model.SnowdriftEvent.Internal
    ( SnowdriftEvent(..)
    ) where

import Model

import Data.Either (Either)
import Data.Int (Int64)
import Data.Time (UTCTime)

-- A sum type of all events, each of which have their own database table.
data SnowdriftEvent
    -- Comment approved.
    = ECommentPosted CommentId Comment

    -- Comment unapproved (pending approval).
    | ECommentPending CommentId Comment

    -- Comment closed (ticket or otherwise)
    | ECommentClosed CommentClosingId CommentClosing

    -- Ticket claimed
    | ETicketClaimed (Either (TicketClaimingId, TicketClaiming) (TicketOldClaimingId, TicketOldClaiming))

    -- Ticket unclaimed
    | ETicketUnclaimed TicketOldClaimingId TicketOldClaiming

    -- Comment rethreaded.
    | ECommentRethreaded RethreadId Rethread -- rethreaded-from-URL

    | ENotificationSent NotificationId Notification

    -- New WikiEdit made.
    | EWikiEdit WikiEditId WikiEdit

    -- New WikiPage posted.
    | EWikiPage WikiPageId WikiPage

    -- New blog post posted.
    | EBlogPost BlogPostId BlogPost

    -- New pledge.
    | ENewPledge SharesPledgedId SharesPledged

    -- Pledge that has changed in value.
    | EUpdatedPledge Int64                         -- old shares
                     SharesPledgedId SharesPledged -- new pledge info

    -- Deleted pledge.
    | EDeletedPledge UTCTime UserId ProjectId Int64
