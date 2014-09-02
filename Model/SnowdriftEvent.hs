module Model.SnowdriftEvent
    ( snowdriftEventNewestToOldest
    , snowdriftEventTime
    ) where

import Import

snowdriftEventNewestToOldest :: SnowdriftEvent -> SnowdriftEvent -> Ordering
snowdriftEventNewestToOldest x y  = compare (snowdriftEventTime y) (snowdriftEventTime x)

snowdriftEventTime :: SnowdriftEvent -> UTCTime
snowdriftEventTime (ECommentPosted _ Comment{..})         = fromMaybe commentCreatedTs commentApprovedTs
snowdriftEventTime (ECommentPending _ Comment{..})        = commentCreatedTs
snowdriftEventTime (ECommentRethreaded _ Rethread{..})    = rethreadTs
snowdriftEventTime (ENotificationSent _ Notification{..}) = notificationCreatedTs
snowdriftEventTime (EWikiEdit _ WikiEdit{..})             = wikiEditTs
snowdriftEventTime (EWikiPage _ WikiPage{..})             = wikiPageCreatedTs
snowdriftEventTime (ENewPledge _ SharesPledged{..})       = sharesPledgedTs
snowdriftEventTime (EUpdatedPledge _ _ SharesPledged{..}) = sharesPledgedTs
snowdriftEventTime (EDeletedPledge ts _ _ _)              = ts
