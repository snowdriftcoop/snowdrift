module Model.Discussion
    ( fetchDiscussionWikiPagesInDB
    ) where

import Import

-- | Given a list of DiscussionId, fetch the discussions which are WikiPages.
fetchDiscussionWikiPagesInDB :: [DiscussionId] -> DB [Entity WikiPage]
fetchDiscussionWikiPagesInDB discussion_ids =
    select $
    from $ \wp -> do
    where_ (wp ^. WikiPageDiscussion `in_` valList discussion_ids)
    return wp
