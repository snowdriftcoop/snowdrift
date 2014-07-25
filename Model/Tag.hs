module Model.Tag
    ( TagMap
    , getAllTags
    , getAllTagsMap
    ) where

import Import

type TagMap = Map TagId Tag

getAllTagsMap :: DB TagMap
getAllTagsMap = entitiesMap <$> getAllTags

getAllTags :: DB [Entity Tag]
getAllTags = select $ from (\t -> return t)
