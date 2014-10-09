module Model.SnowdriftEvent
    ( snowdriftEventNewestToOldest
    , snowdriftEventTime
    , snowdriftEventToFeedEntry
    ) where

import Import

import Model.Discussion
import Model.User

import qualified Data.Map   as M
import qualified Data.Text  as T
import           Yesod.Feed (FeedEntry(..))

snowdriftEventNewestToOldest :: SnowdriftEvent -> SnowdriftEvent -> Ordering
snowdriftEventNewestToOldest x y  = compare (snowdriftEventTime y) (snowdriftEventTime x)

snowdriftEventTime :: SnowdriftEvent -> UTCTime
snowdriftEventTime (ECommentPosted _ Comment{..})         = fromMaybe commentCreatedTs commentApprovedTs
snowdriftEventTime (ECommentPending _ Comment{..})        = commentCreatedTs
snowdriftEventTime (ECommentRethreaded _ Rethread{..})    = rethreadTs
snowdriftEventTime (ENotificationSent _ Notification{..}) = notificationCreatedTs
snowdriftEventTime (EWikiEdit _ WikiEdit{..})             = wikiEditTs
snowdriftEventTime (EWikiPage _ WikiPage{..})             = wikiPageCreatedTs
snowdriftEventTime (EBlogPost _ BlogPost{..})             = blogPostTs
snowdriftEventTime (ENewPledge _ SharesPledged{..})       = sharesPledgedTs
snowdriftEventTime (EUpdatedPledge _ _ SharesPledged{..}) = sharesPledgedTs
snowdriftEventTime (EDeletedPledge ts _ _ _)              = ts

-- Eventually the html rendering here should be moved to the top level somewhere for sharing with notifications
snowdriftEventToFeedEntry
        :: (Route App -> Text)
        -> Text
        -> Map UserId User
        -> Map DiscussionId DiscussionOn
        -> Map WikiPageId WikiPage
        -> SnowdriftEvent
        -> Maybe (FeedEntry (Route App))
snowdriftEventToFeedEntry render project_handle user_map discussion_map _ (ECommentPosted comment_id comment) =
    let user_id    = commentUser comment
        maybe_user = M.lookup user_id user_map
        username   = maybe "<unknown user>" (userDisplayName . Entity user_id) maybe_user
        discussion = case M.lookup (commentDiscussion comment) discussion_map of
            Nothing                                          -> "<unknown discussion>"
            Just (DiscussionOnProject _)                     -> "project discussion"
            Just (DiscussionOnWikiPage (Entity _ wiki_page)) -> "wiki discussion for \"" <> wikiPageTarget wiki_page <> "\""
     in Just $ FeedEntry
            { feedEntryLink    = CommentDirectLinkR comment_id
            , feedEntryUpdated = maybe (commentCreatedTs comment) id $ commentApprovedTs comment
            , feedEntryTitle   = T.unwords [ T.snoc project_handle ':', "new comment posted on", discussion, "by", username ]
            , feedEntryContent = [hamlet| |] render
            }

snowdriftEventToFeedEntry render project_handle _ _ _ (ECommentRethreaded _ rethread) =
    Just $ FeedEntry
        { feedEntryLink    = CommentDirectLinkR $ rethreadNewComment rethread
        , feedEntryUpdated = rethreadTs rethread
        , feedEntryTitle   = T.unwords [ T.snoc project_handle ':', "comment rethreaded" ]
        , feedEntryContent = [hamlet| |] render
        }

snowdriftEventToFeedEntry render project_handle _ _ _ (EWikiPage _ wiki_page) =
    let target = wikiPageTarget wiki_page
     in Just $ FeedEntry
            { feedEntryLink    = WikiR project_handle $ wikiPageTarget wiki_page
            , feedEntryUpdated = wikiPageCreatedTs wiki_page
            , feedEntryTitle   = T.unwords [ T.snoc project_handle ':', "new wiki page", "\"" <> target <> "\"" ]
            , feedEntryContent = [hamlet| |] render
            }

snowdriftEventToFeedEntry render project_handle user_map _ wiki_page_map (EWikiEdit wiki_edit_id wiki_edit) =
    let maybe_wiki_page = M.lookup (wikiEditPage wiki_edit) wiki_page_map
        target          = maybe (error "missing wiki page for edit") wikiPageTarget maybe_wiki_page
        user_id         = wikiEditUser wiki_edit
        maybe_user      = M.lookup user_id user_map
        username        = maybe "<unknown user>" (userDisplayName . Entity user_id) maybe_user

     in Just $ FeedEntry
            { feedEntryLink    = WikiEditR project_handle target wiki_edit_id
            , feedEntryUpdated = wikiEditTs wiki_edit
            , feedEntryTitle   = T.unwords [ T.snoc project_handle ':', "wiki page", "\"" <> target <> "\"", "edited by", username ]
            , feedEntryContent = [hamlet| |] render
            }

snowdriftEventToFeedEntry render project_handle _ _ _
        ( EBlogPost _
            BlogPost
                { blogPostHandle = handle
                , blogPostTs = ts
                , blogPostTitle = title
                }
        ) =
    Just $ FeedEntry
        { feedEntryLink    = ProjectBlogR handle
        , feedEntryUpdated = ts
        , feedEntryTitle   = T.unwords [ T.snoc project_handle ':', "new blog post:", "\"" <> title <> "\"" ]
        , feedEntryContent = [hamlet| |] render
        }


-- We might want to show these, but I'm not sure.  Leaving them out now, at any rate.
snowdriftEventToFeedEntry _ _ _ _ _ (ENewPledge _ _)         = Nothing
snowdriftEventToFeedEntry _ _ _ _ _ (EUpdatedPledge _ _ _)   = Nothing
snowdriftEventToFeedEntry _ _ _ _ _ (EDeletedPledge _ _ _ _) = Nothing

-- Graveyard of event types we don't want to put on the feed.
-- Don't match-all here, we don't want to accidentally not consider something.

snowdriftEventToFeedEntry _ _ _ _ _ (ENotificationSent _ _) = Nothing
snowdriftEventToFeedEntry _ _ _ _ _ (ECommentPending _ _)   = Nothing
