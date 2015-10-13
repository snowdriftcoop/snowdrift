module Model.Discussion
    ( DiscussionOn(..)
    , DiscussionType(..)
    , createDiscussionDB
    , fetchDiscussionClosedOrRetractedRootCommentsDB
    , fetchDiscussionDB
    , fetchDiscussionsDB
    , fetchDiscussionRootCommentsDB
    ) where

import Import

import Control.Monad.Trans.Maybe
import Data.List (sortBy)
import Model.Comment.Sql
import qualified Data.Map as M
import qualified Data.Set as S

-- | Sort targets by language preference
langPref :: [Language] -> [Entity WikiTarget] -> [Entity WikiTarget]
langPref langs =
    sortBy (languagePreferenceOrder langs (wikiTargetLanguage . entityVal))

-- | Given a 'requested' DiscussionType, attempt to fetch the Discussion from
-- that table. If, say, the requested DiscussionType is DiscussionTypeProject,
-- but the given DiscussionId corresponds to a discussion on a WikiPage, this
-- function will return Nothing.
--
-- TODO(mitchell): Make this function more type safe.
fetchDiscussionInternal
    :: [Language]
    -> DiscussionId
    -> DiscussionType
    -> DB (Maybe DiscussionOn)
fetchDiscussionInternal _ discussion_id DiscussionTypeProject =
    fmap (fmap DiscussionOnProject) $
        getBy $ UniqueProjectDiscussion discussion_id
fetchDiscussionInternal _ discussion_id DiscussionTypeBlogPost =
    fmap (fmap DiscussionOnBlogPost) $
        getBy $ UniqueBlogPostDiscussion discussion_id
fetchDiscussionInternal langs discussion_id DiscussionTypeWikiPage = do
    maybe_wiki_page <- getBy $ UniqueWikiPageDiscussion discussion_id
    case maybe_wiki_page of
        Nothing -> return Nothing
        Just (Entity wiki_page_id _) -> do
            targets <- select $ from $ \wt -> do
                   where_ (wt ^. WikiTargetPage ==. val wiki_page_id)
                   return wt

            case langPref langs targets of
                [] -> return Nothing
                target:_ -> return $ Just $ DiscussionOnWikiPage target

fetchDiscussionInternal _ discussion_id DiscussionTypeUser =
    fmap (fmap DiscussionOnUser) $ getBy $ UniqueUserDiscussion discussion_id


fetchDiscussionsInternal
    :: [Language]
    -> [DiscussionId]
    -> DiscussionType
    -> DB (Map DiscussionId DiscussionOn)
fetchDiscussionsInternal _ discussion_ids DiscussionTypeProject =
    fmap (foldr go mempty) $ select $ from $ \p -> do
        where_ $ p ^. ProjectDiscussion `in_` valList discussion_ids
        return p
  where
    go :: Entity Project
       -> Map DiscussionId DiscussionOn
       -> Map DiscussionId DiscussionOn
    go p@(Entity _ Project{..}) =
        M.insert projectDiscussion (DiscussionOnProject p)

fetchDiscussionsInternal _ discussion_ids DiscussionTypeBlogPost =
    fmap (foldr go mempty) $
    select $
    from $ \bp -> do
    where_ $ bp ^. BlogPostDiscussion `in_` valList discussion_ids
    return bp
  where
    go :: Entity BlogPost
       -> Map DiscussionId DiscussionOn
       -> Map DiscussionId DiscussionOn
    go p@(Entity _ BlogPost{..}) =
        M.insert blogPostDiscussion (DiscussionOnBlogPost p)

fetchDiscussionsInternal langs discussion_ids DiscussionTypeWikiPage = do
    wiki_pages <-
        select $
        from $ \wp -> do
        where_ $ wp ^. WikiPageDiscussion `in_` valList discussion_ids
        return wp

    wiki_targets <-
        select $
        from $ \wt -> do
        where_ $ wt ^. WikiTargetPage `in_` valList (map entityKey wiki_pages)
        return wt

    let wiki_target_map =
            M.mapMaybe
                (listToMaybe . langPref langs)
                (M.fromListWith
                    (++)
                    (map (wikiTargetPage . entityVal &&& (:[])) wiki_targets))

    return $
        M.fromList $
            mapMaybe
                (\wiki_page ->
                    fmap
                        ((wikiPageDiscussion $ entityVal wiki_page,)
                            . DiscussionOnWikiPage)
                        (M.lookup (entityKey wiki_page) wiki_target_map))
                wiki_pages

fetchDiscussionsInternal _ discussion_ids DiscussionTypeUser =
    fmap (foldr go mempty) $ select $ from $ \u -> do
        where_ $ u ^. UserDiscussion `in_` valList discussion_ids
        return u
  where
    go :: Entity User
       -> Map DiscussionId DiscussionOn
       -> Map DiscussionId DiscussionOn
    go u@(Entity _ User{..}) = M.insert userDiscussion (DiscussionOnUser u)

-- | Fetch a single discussion, given its id.
fetchDiscussionDB :: [Language] -> DiscussionId -> DB DiscussionOn
fetchDiscussionDB langs discussion_id =
    fromJustErr "fetchDiscussionDB: discussion not found" <$> runMaybeT (msum f)
  where
    -- f :: [MaybeT DB DiscussionOn]
    f = map (MaybeT . fetchDiscussionInternal langs discussion_id)
            [minBound..maxBound]
    fromJustErr _ (Just x) = x
    fromJustErr msg _ = error msg


-- | Fetch a list of discussions, given their ids. The returned map will have a
-- key for every input DiscussionId.
fetchDiscussionsDB
    :: [Language]
    -> [DiscussionId]
    -> DB (Map DiscussionId DiscussionOn)
fetchDiscussionsDB langs discussion_ids = do
    discussion_map <-
        mconcat <$> mapM (fetchDiscussionsInternal langs discussion_ids)
                         [minBound..maxBound]

    let missed_discussions =
            S.fromList discussion_ids S.\\ M.keysSet discussion_map

    unless (S.null missed_discussions) $
        error $
            "fetchDiscussionsDB: some discussion not found: "
            ++ show missed_discussions

    return discussion_map

--------------------------------------------------------------------------------

createDiscussionDB :: DB DiscussionId
createDiscussionDB = insert (Discussion 0)

-- | Get all open root Comments on a Discussion.
fetchDiscussionRootCommentsDB
    :: DiscussionId
    -> ExprCommentCond
    -> DB [Entity Comment]
fetchDiscussionRootCommentsDB = fetchRootComments exprCommentOpen

-- | Get all closed root Comments on a Discussion.
fetchDiscussionClosedOrRetractedRootCommentsDB
    :: DiscussionId
    -> ExprCommentCond
    -> DB [Entity Comment]
fetchDiscussionClosedOrRetractedRootCommentsDB =
    fetchRootComments exprCommentClosedOrRetracted

fetchRootComments
    :: ExprCommentCond
    -> DiscussionId
    -> ExprCommentCond
    -> DB [Entity Comment]
fetchRootComments open_or_closed discussion_id has_permission =
    select $
    from $ \c -> do
    where_ $
        exprCommentOnDiscussion discussion_id c &&.
        exprCommentIsRoot c &&.
        open_or_closed c &&.
        has_permission c
    return c
