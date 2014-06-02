
module Model.AnnotatedTag where

import Import

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L

import Data.List (sortBy)

import Text.Printf

data AnnotatedTag = AnnotatedTag
    { atTag :: Entity Tag
    , atUrl :: Route App
    , atColor :: Color
    , atUserVotes :: [(Entity User, Int)]
    }

atName :: AnnotatedTag -> Text
atName = tagName . entityVal . atTag

{- Scoring for voting on tags is something not currently presented
- on the site. We've discussed changing it. I (Aaron) prefer a 6-point
- range voting just like we proposed for the Bylaws instead of mimicking
- the pledge formula here. Final decisions haven't been made yet -}

atScore :: AnnotatedTag -> Double
atScore = sum . map (\ (_, x) -> if x == 0 then 0 else fromIntegral (signum x) * logBase 2 (1 + fromIntegral (abs x) :: Double)) . atUserVotes

atUserScore :: AnnotatedTag -> UserId -> Maybe Int
atUserScore at user_id = fmap snd $ L.find ((== user_id) . entityKey . fst) $ atUserVotes at

atScoreString :: AnnotatedTag -> String
atScoreString = printf "%.1f" . atScore


buildAnnotatedTags :: Map TagId Tag -> (TagId -> Route App) -> [(TagId, (UserId, Int))] -> Handler [AnnotatedTag]
buildAnnotatedTags tag_map tagUrl tags = do
    user_map <- fmap (M.fromList . entityPairs) $ runDB $ select $ from $ \ user -> do
        where_ $ user ^. UserId `in_` valList (S.toList $ S.fromList $ map (fst . snd) tags)
        return user

    tag_colors <- fmap (M.mapKeysMonotonic Key) $ cached $ fmap M.fromList $ do
        maybe_user_id <- maybeAuthId
        case maybe_user_id of
            Nothing -> do
                colors <- runDB $ select $ from return
                return $ map ((unKey . defaultTagColorTag &&& Color . defaultTagColorColor) . entityVal) colors

            Just user_id -> do
                colors <- runDB $ select $ from $ \ tag_color -> do
                    where_ $ tag_color ^. TagColorUser ==. val user_id
                    return tag_color

                return $ map ((unKey . tagColorTag &&& Color . tagColorColor) . entityVal) colors

    let merged_tags = M.toList $ M.fromListWith (++) $ map (second return) tags

    annotated_tags <- forM merged_tags $ \ (tag_id, user_votes) -> do
        let tag = Entity tag_id $ tag_map M.! tag_id
            user_votes' :: [(Entity User, Int)]
            user_votes' = map (first (uncurry Entity . (id &&& (user_map M.!)))) user_votes
            sorted_user_votes :: [(Entity User, Int)]
            sorted_user_votes = sortBy (compare `on` (userName . entityVal . fst)) user_votes'

        return $ AnnotatedTag tag (tagUrl tag_id) (M.findWithDefault 0x77AADD tag_id tag_colors) sorted_user_votes
        
    return $ sortBy (compare `on` atScore) annotated_tags

annotateCommentTags :: Map TagId Tag -> Text -> Text -> CommentId -> [CommentTag] -> Handler [AnnotatedTag]
annotateCommentTags tag_map project_handle target comment_id =
    buildAnnotatedTags tag_map (CommentTagR project_handle target comment_id) . map (commentTagTag &&& (commentTagUser &&& commentTagCount))

