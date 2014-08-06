module Model.SnowdriftEvent.Internal
    ( SnowdriftEvent(..)
    ) where

import Model

-- A sum type of all events, each of which have their own database table.
data SnowdriftEvent
    = ECommentPosted  CommentId Comment   -- Comment approved.
    | ECommentPending CommentId Comment   -- Comment unapproved (pending approval).
    | EMessageSent    MessageId Message
    | EWikiEdit       WikiEditId WikiEdit -- New WikiEdit made.
    | EWikiPage       WikiPageId WikiPage -- New WikiPage posted.
