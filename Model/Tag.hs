module Model.Tag
    ( TagMap
    , fetchTagsInDB
    , getAllTags
    , getAllTagsMap
    ) where

import Import

type TagMap = Map TagId Tag

fetchTagsInDB :: [TagId] -> DB [Entity Tag]
fetchTagsInDB tag_ids =
    select $
        from $ \t -> do
        where_ (t ^. TagId `in_` valList tag_ids)
        return t

getAllTagsMap :: DB TagMap
getAllTagsMap = entitiesMap <$> getAllTags

getAllTags :: DB [Entity Tag]
getAllTags = select $ from (\t -> return t)
