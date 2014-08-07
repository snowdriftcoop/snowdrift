module Model.SnowdriftEvent
    ( snowdriftEventNewestToOldest
    ) where

import Import

snowdriftEventNewestToOldest :: SnowdriftEvent -> SnowdriftEvent -> Ordering
snowdriftEventNewestToOldest x y  = compare (snowdriftEventTime y) (snowdriftEventTime x)

snowdriftEventTime :: SnowdriftEvent -> UTCTime
snowdriftEventTime (ECommentPosted _ Comment{..})         = fromMaybe commentCreatedTs commentModeratedTs
snowdriftEventTime (ECommentPending _ Comment{..})        = commentCreatedTs
snowdriftEventTime (EMessageSent _ Message{..})           = messageCreatedTs
snowdriftEventTime (EWikiEdit _ WikiEdit{..})             = wikiEditTs
snowdriftEventTime (EWikiPage _ WikiPage{..})             = wikiPageCreatedTs
snowdriftEventTime (ENewPledge _ SharesPledged{..})       = sharesPledgedTs
snowdriftEventTime (EUpdatedPledge _ _ SharesPledged{..}) = sharesPledgedTs
snowdriftEventTime (EDeletedPledge ts _ _ _)              = ts
