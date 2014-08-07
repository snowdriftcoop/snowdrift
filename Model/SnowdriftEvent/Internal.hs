module Model.SnowdriftEvent.Internal
    ( SnowdriftEvent(..)
    ) where

import Model

import Data.Int (Int64)
import Data.Time (UTCTime)

-- A sum type of all events, each of which have their own database table.
data SnowdriftEvent
    -- Comment approved.
    = ECommentPosted  CommentId Comment
    -- Comment unapproved (pending approval).
    | ECommentPending CommentId Comment
    | EMessageSent MessageId Message
    -- New WikiEdit made.
    | EWikiEdit WikiEditId WikiEdit
    -- New WikiPage posted.
    | EWikiPage WikiPageId WikiPage
    -- New pledge.
    | ENewPledge SharesPledgedId SharesPledged
    -- Pledge that has changed in value.
    | EUpdatedPledge Int64 {- old shares -}
                     SharesPledgedId SharesPledged {- new pledge info -}
    -- Deleted pledge.
    | EDeletedPledge UTCTime UserId ProjectId Int64
