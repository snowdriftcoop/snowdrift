module Model.Tag
    ( AnnotatedTag(..)
    , Color(..)
    , annotTagName
    , annotTagScore
    , annotTagScoreString
    , annotTagUserScore
    , sortAnnotTagsByName
    , sortAnnotTagsByScore
    , fetchAllTagsDB
    , fetchTagsInDB
    , fetchTagColorsDB
    , fetchDefaultTagColorsDB
    ) where

import Import hiding (Color)

import Data.List (sortBy)
import Text.Printf
import qualified Data.List as L
import qualified Data.Map as M

newtype Color = Color Int deriving (Num)

fetchAllTagsDB :: DB [Entity Tag]
fetchAllTagsDB = select (from return)

fetchTagsInDB :: [TagId] -> DB [Entity Tag]
fetchTagsInDB tag_ids =
    select $
        from $ \t -> do
        where_ (t ^. TagId `in_` valList tag_ids)
        return t

fetchTagColorsDB :: UserId -> DB (Map TagId Color)
fetchTagColorsDB user_id = fmap go $
    select $
    from $ \tc -> do
    where_ (tc ^. TagColorUser ==. val user_id)
    return tc
  where
    go :: [Entity TagColor] -> Map TagId Color
    go = M.fromList . map ((tagColorTag &&& Color . tagColorColor) . entityVal)

fetchDefaultTagColorsDB :: DB (Map TagId Color)
fetchDefaultTagColorsDB = go <$> select (from return)
  where
    go :: [Entity DefaultTagColor] -> Map TagId Color
    go =
        M.fromList
        . map (
            (defaultTagColorTag &&& Color . defaultTagColorColor)
            . entityVal)

-- | An tag 'annotated' with rendering information..
data AnnotatedTag = AnnotatedTag
    { annotTagTag       :: Entity Tag
    , annotTagUrl       :: Route App
    -- ^ The route to POST to (for changing votes).
    , annotTagColor     :: Color
    , annotTagUserVotes :: [(Entity User, Int)]
    }

annotTagName :: AnnotatedTag -> Text
annotTagName = tagName . entityVal . annotTagTag

{- Scoring for voting on tags is something not currently presented
- on the site. We've discussed changing it. I (Aaron) prefer a 6-point
- range voting just like we proposed for the Bylaws instead of mimicking
- the pledge formula here. Final decisions haven't been made yet -}

annotTagScore :: AnnotatedTag -> Double
annotTagScore =
    sum
    . map (\(_, x) ->
        if x == 0
            then 0
            else fromIntegral
                (signum x)
                * logBase 2 (1 + fromIntegral (abs x) :: Double))
    . annotTagUserVotes

annotTagUserScore :: AnnotatedTag -> UserId -> Maybe Int
annotTagUserScore at user_id =
    fmap snd $ L.find ((== user_id) . entityKey . fst) $ annotTagUserVotes at

annotTagScoreString :: AnnotatedTag -> String
annotTagScoreString = printf "%.1f" . annotTagScore

sortAnnotTagsByName :: [AnnotatedTag] -> [AnnotatedTag]
sortAnnotTagsByName = sortBy (compare `on` annotTagName)

sortAnnotTagsByScore :: [AnnotatedTag] -> [AnnotatedTag]
sortAnnotTagsByScore = sortBy (compare `on` annotTagScore)
