module Model.Tag
    ( TagMap
    , getAllTags
    , getAllTagsMap
    ) where

import Import

type TagMap = Map TagId Tag

getAllTagsMap :: YesodDB App TagMap
getAllTagsMap = entitiesMap <$> getAllTags

getAllTags :: YesodDB App [Entity Tag]
getAllTags = select $ from (\t -> return t)
