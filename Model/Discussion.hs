module Model.Discussion
    ( createDiscussionDB
    , fetchDiscussionProjectDB
    , fetchDiscussionWikiPage
    , fetchDiscussionWikiPagesInDB
    ) where

import Import

import Control.Monad.Trans.Maybe

-- | Given a list of DiscussionId, fetch the discussions which are WikiPages.
fetchDiscussionWikiPagesInDB :: [DiscussionId] -> DB [Entity WikiPage]
fetchDiscussionWikiPagesInDB discussion_ids =
    select $
    from $ \wp -> do
    where_ (wp ^. WikiPageDiscussion `in_` valList discussion_ids)
    return wp

-- | Fetch the Project this Discussion is associated with (if any).
-- TODO(mitchell): Does this require constant attention, as we expand
-- discussions?
fetchDiscussionProjectDB :: DiscussionId -> DB (Maybe ProjectId)
fetchDiscussionProjectDB discussion_id = runMaybeT $
    -- From a list of possible ways to find a ProjectId from a DiscussionId, find the Project (maybe).
    foldr (mplus . f) mzero
        -- add more functions here as necessary
        [(fetchDiscussionWikiPage, wikiPageProject)]
  where
    -- f :: (DiscussionId -> DB (Maybe (Entity a)), a -> ProjectId) -> MaybeT DB (Entity Project)
    f (action, project_id_getter) = project_id_getter . entityVal <$> MaybeT (action discussion_id)

-- | Fetch the WikiPage this Discussion is on with (if any).
fetchDiscussionWikiPage :: DiscussionId -> DB (Maybe (Entity WikiPage))
fetchDiscussionWikiPage discussion_id = fmap listToMaybe $
    select $
    from $ \wp -> do
    where_ (wp ^. WikiPageDiscussion ==. val discussion_id)
    return wp

createDiscussionDB :: DB DiscussionId
createDiscussionDB = insert (Discussion 0)
