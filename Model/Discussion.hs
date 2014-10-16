module Model.Discussion
    ( DiscussionOn(..)
    , createDiscussionDB
    , fetchDiscussionClosedOrRetractedRootCommentsDB
    , fetchDiscussionDB
    , fetchDiscussionsDB
    , fetchDiscussionRootCommentsDB
    ) where

import Import

import Model.Comment.Sql
import Control.Monad.Trans.Maybe

import qualified Data.Map as M

-- | An internal sum type that contains a constructer per database table that acts
-- as a "Discussion". This way, we get a relatively type-safe way of ensuring that
-- all possible such tables are referenced when fetching the data associated with
-- some arbitrary Discussion.
--
-- Any new Discussion-ey tables MUST have a constructor added here! (and below, too)
data DiscussionType
    = DiscussionTypeProject
    | DiscussionTypeWikiPage
    | DiscussionTypeUser
    deriving (Bounded, Enum)

-- | Similar to DiscussionType, but exported, and actually contains the data.
data DiscussionOn
    = DiscussionOnProject  (Entity Project)
    | DiscussionOnWikiPage (Entity WikiPage)
    | DiscussionOnUser     (Entity User)

-- | Given a 'requested' DiscussionType, attempt to fetch the Discussion from that
-- table. If, say, the requested DiscussionType is DiscussionTypeProject, but the
-- given DiscussionId corresponds to a discussion on a WikiPage, this function
-- will return Nothing.
--
-- TODO(mitchell): Make this function more type safe.
fetchDiscussionInternal :: DiscussionId -> DiscussionType -> DB (Maybe DiscussionOn)
fetchDiscussionInternal discussion_id DiscussionTypeProject = fmap (fmap DiscussionOnProject . listToMaybe) $
    select $
    from $ \p -> do
    where_ (p ^. ProjectDiscussion ==. val discussion_id)
    return p

fetchDiscussionInternal discussion_id DiscussionTypeWikiPage = fmap (fmap DiscussionOnWikiPage . listToMaybe) $
    select $
    from $ \wp -> do
    where_ (wp ^. WikiPageDiscussion ==. val discussion_id)
    return wp

fetchDiscussionInternal discussion_id DiscussionTypeUser = fmap (fmap DiscussionOnUser . listToMaybe) $
    select $
    from $ \u -> do
    where_ (u ^. UserDiscussion ==. val discussion_id)
    return u


fetchDiscussionsInternal :: [DiscussionId] -> DiscussionType -> DB (Map DiscussionId DiscussionOn)
fetchDiscussionsInternal discussion_ids DiscussionTypeProject = fmap (foldr go mempty) $
    select $
    from $ \p -> do
    where_ (p ^. ProjectDiscussion `in_` valList discussion_ids)
    return p
  where
    go :: Entity Project -> Map DiscussionId DiscussionOn -> Map DiscussionId DiscussionOn
    go p@(Entity _ Project{..}) = M.insert projectDiscussion (DiscussionOnProject p)

fetchDiscussionsInternal discussion_ids DiscussionTypeWikiPage = fmap (foldr go mempty) $
    select $
    from $ \wp -> do
    where_ (wp ^. WikiPageDiscussion `in_` valList discussion_ids)
    return wp
  where
    go :: Entity WikiPage -> Map DiscussionId DiscussionOn -> Map DiscussionId DiscussionOn
    go w@(Entity _ WikiPage{..}) = M.insert wikiPageDiscussion (DiscussionOnWikiPage w)

fetchDiscussionsInternal discussion_ids DiscussionTypeUser = fmap (foldr go mempty) $
    select $
    from $ \u -> do
    where_ (u ^. UserDiscussion `in_` valList discussion_ids)
    return u
  where
    go :: Entity User -> Map DiscussionId DiscussionOn -> Map DiscussionId DiscussionOn
    go u@(Entity _ User{..}) = M.insert userDiscussion (DiscussionOnUser u)

-- | Fetch a single discussion, given its id.
fetchDiscussionDB :: DiscussionId -> DB DiscussionOn
fetchDiscussionDB discussion_id =
    fromJustErr "fetchDiscussionDB: discussion not found" <$> runMaybeT (foldr mplus mzero f)
  where
    -- f :: [MaybeT DB DiscussionOn]
    f = map (MaybeT . fetchDiscussionInternal discussion_id) [minBound..maxBound]

-- | Fetch a list of discussions, given their ids. The returned map will have a key for
-- every input DiscussionId.
fetchDiscussionsDB :: [DiscussionId] -> DB (Map DiscussionId DiscussionOn)
fetchDiscussionsDB discussion_ids = do
    discussion_map <- mconcat <$> sequence (map (fetchDiscussionsInternal discussion_ids) [minBound..maxBound])
    when (M.size discussion_map /= length discussion_ids) $
        error "fetchDiscussionsDB: some discussion not found"
    return discussion_map

--------------------------------------------------------------------------------

createDiscussionDB :: DB DiscussionId
createDiscussionDB = insert (Discussion 0)

-- | Get all open root Comments on a Discussion.
fetchDiscussionRootCommentsDB :: DiscussionId -> ExprCommentCond -> DB [Entity Comment]
fetchDiscussionRootCommentsDB = fetchRootComments exprCommentOpen

-- | Get all closed root Comments on a Discussion.
fetchDiscussionClosedOrRetractedRootCommentsDB :: DiscussionId -> ExprCommentCond -> DB [Entity Comment]
fetchDiscussionClosedOrRetractedRootCommentsDB = fetchRootComments exprCommentClosedOrRetracted

fetchRootComments :: ExprCommentCond -> DiscussionId -> ExprCommentCond -> DB [Entity Comment]
fetchRootComments open_or_closed discussion_id has_permission =
    select $
    from $ \c -> do
    where_ $
        exprCommentOnDiscussion discussion_id c &&.
        exprCommentIsRoot c &&.
        open_or_closed c &&.
        has_permission c
    return c
