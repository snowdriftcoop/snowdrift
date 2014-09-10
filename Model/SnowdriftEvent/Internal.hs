module Model.SnowdriftEvent.Internal
    ( SnowdriftEvent(..)
    ) where

import Model

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
    -- Comment rethreaded.
    | ECommentRethreaded RethreadId Rethread -- rethreaded-from-URL
    | ENotificationSent NotificationId Notification
    -- New WikiEdit made.
    | EWikiEdit WikiEditId WikiEdit
    -- New WikiPage posted.
    | EWikiPage WikiPageId WikiPage
    -- New pledge.
    | ENewPledge SharesPledgedId SharesPledged
    -- Pledge that has changed in value.
    | EUpdatedPledge Int64                         -- old shares
                     SharesPledgedId SharesPledged -- new pledge info
    -- Deleted pledge.
    | EDeletedPledge UTCTime UserId ProjectId Int64
