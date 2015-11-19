module Model.Tag
    ( AnnotatedTag(..)
    , Color(..)
    , annotTagName
    , annotTagScore
    , annotTagScoreString
    , annotTagUserScore
    , buildAnnotatedCommentTagsDB
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

-- | Annotate a [CommentTag]. Returns a Map CommentId [AnnotatedTag] so this
-- function can be called with multiple Comments' CommentTags. If all
-- [CommentTag] are of the same comment, that's fine -- the returned map will
-- only have one key.
--
-- The [AnnotatedTag] value is left unsorted, but the [(Entity User, Int)]
-- within each AnnotatedTag is sorted by ascending username.
buildAnnotatedCommentTagsDB
    :: Maybe UserId
    -> [CommentTag]
    -> DB (Map CommentId [AnnotatedTag])
buildAnnotatedCommentTagsDB muser_id comment_tags =  do
    let user_ids = map commentTagUser comment_tags
    user_map <- entitiesMap <$> selectList [UserId <-. user_ids] []
    tag_map  <- entitiesMap <$> fetchTagsInDB (map commentTagTag comment_tags)

    -- TODO(mitchell): cached
    tag_colors <- maybe fetchDefaultTagColorsDB fetchTagColorsDB muser_id

    let f :: [CommentTag] -> Map CommentId [AnnotatedTag]
        f = M.mapWithKey (map . i) . M.map h . g

        -- Pair each CommentTag with its CommentId, then collect
        -- CommentTags back up, grouped by their CommentIds.
        g :: [CommentTag] -> Map CommentId [CommentTag]
        g = M.fromListWith (++) . map (commentTagComment &&& return)

        -- Group each CommentTag by TagId, combining Users' votes.
        h :: [CommentTag] -> [(TagId, [(Entity User, Int)])]
        h = M.toList . foldr step mempty
          where
            step :: CommentTag
                 -> Map TagId [(Entity User, Int)]
                 -> Map TagId [(Entity User, Int)]
            step (CommentTag _ tag_id user_id n) =
                M.insertWith (++)
                             tag_id
                             [(Entity user_id (user_map M.! user_id), n)]

        -- Construct an AnnotatedTag given all relevant info.
        i :: CommentId -> (TagId, [(Entity User, Int)]) -> AnnotatedTag
        i comment_id (tag_id, user_votes) =
          AnnotatedTag
            (Entity tag_id (tag_map M.! tag_id))
            (CommentTagR comment_id tag_id)
            (M.findWithDefault 0x77AADD tag_id tag_colors)
            (sortBy (compare `on` (userName . entityVal . fst)) user_votes)

    return (f comment_tags)
