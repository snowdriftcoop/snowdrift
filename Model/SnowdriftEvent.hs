{-# LANGUAGE RecordWildCards #-}

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
snowdriftEventTime (ECommentPosted _ Comment{..})          = fromMaybe commentCreatedTs commentApprovedTs
snowdriftEventTime (ECommentPending _ Comment{..})         = commentCreatedTs
snowdriftEventTime (ECommentRethreaded _ Rethread{..})     = rethreadTs
snowdriftEventTime (ECommentClosed _ CommentClosing{..})   = commentClosingTs
snowdriftEventTime (ETicketClaimed _ TicketClaiming{..})   = ticketClaimingTs
snowdriftEventTime (ETicketUnclaimed _ TicketClaiming{..}) = fromMaybe (error "TicketUnclaimed event for TicketClaiming with no ReleasedTs") ticketClaimingReleasedTs
snowdriftEventTime (ENotificationSent _ Notification{..})  = notificationCreatedTs
snowdriftEventTime (EWikiEdit _ WikiEdit{..})              = wikiEditTs
snowdriftEventTime (EWikiPage _ WikiPage{..})              = wikiPageCreatedTs
snowdriftEventTime (EBlogPost _ BlogPost{..})              = blogPostTs
snowdriftEventTime (ENewPledge _ SharesPledged{..})        = sharesPledgedTs
snowdriftEventTime (EUpdatedPledge _ _ SharesPledged{..})  = sharesPledgedTs
snowdriftEventTime (EDeletedPledge ts _ _ _)               = ts

-- Eventually the html rendering here should be moved to the top level somewhere for sharing with notifications
snowdriftEventToFeedEntry
        :: (Route App -> Text)
        -> Text
        -- -> Prefetch
        -> Map UserId User
        -> Map DiscussionId DiscussionOn
        -> Map WikiPageId WikiPage
        -> Map CommentId (Entity Ticket)
        -> SnowdriftEvent
        -> Maybe (FeedEntry (Route App))
snowdriftEventToFeedEntry render project_handle user_map discussion_map _ _ (ECommentPosted comment_id comment) =
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

snowdriftEventToFeedEntry render project_handle _ _ _ _ (ECommentRethreaded _ rethread) =
    Just $ FeedEntry
        { feedEntryLink    = CommentDirectLinkR $ rethreadNewComment rethread
        , feedEntryUpdated = rethreadTs rethread
        , feedEntryTitle   = T.unwords [ T.snoc project_handle ':', "comment rethreaded" ]
        , feedEntryContent = [hamlet| |] render
        }

snowdriftEventToFeedEntry render project_handle user_map _ _ ticket_map (ECommentClosed _ CommentClosing{..}) =
    let user_id    = commentClosingClosedBy
        maybe_user = M.lookup user_id user_map
        username   = maybe "<unknown user>" (userDisplayName . Entity user_id) maybe_user

        mk_feed_entry title = Just $ FeedEntry
            { feedEntryLink    = CommentDirectLinkR commentClosingComment
            , feedEntryUpdated = commentClosingTs
            , feedEntryTitle   = title
            , feedEntryContent = [hamlet| |] render
            }

     in case M.lookup commentClosingComment ticket_map of
            Just (Entity ticket_id Ticket{..}) ->
                let ticket_str = case ticket_id of
                        Key (PersistInt64 tid) -> T.pack $ show tid
                        Key _ -> "<malformed id>"
                 in mk_feed_entry $ T.unwords
                        [ T.snoc project_handle ':'
                        , "ticket closed by"
                        , T.snoc username ':'
                        , T.concat [ "SD-", ticket_str, ":" ]
                        , ticketName
                        ]

            Nothing -> mk_feed_entry $ T.unwords
                        [ T.snoc project_handle ':'
                        , "comment thread closed by"
                        , username
                        ]

snowdriftEventToFeedEntry render project_handle user_map _ _ ticket_map (ETicketClaimed _ TicketClaiming{..}) =
    let user_id    = ticketClaimingUser
        maybe_user = M.lookup user_id user_map
        username   = maybe "<unknown user>" (userDisplayName . Entity user_id) maybe_user
        Entity ticket_id Ticket{..} = lookupErr "snowdriftEventToFeedEntry: comment id not present in ticket map" ticketClaimingTicket ticket_map
        ticket_str = case ticket_id of
            Key (PersistInt64 tid) -> T.pack $ show tid
            Key _ -> "<malformed id>"

     in Just $ FeedEntry
            { feedEntryLink    = CommentDirectLinkR ticketClaimingTicket
            , feedEntryUpdated = ticketClaimingTs
            , feedEntryTitle   = T.unwords
                [ T.snoc project_handle ':'
                , "ticket claimed by"
                , T.snoc username ':'
                , T.concat [ "SD-", ticket_str, ":" ]
                , ticketName
                ]
            , feedEntryContent = [hamlet| |] render
            }

snowdriftEventToFeedEntry render project_handle _ _ _ ticket_map (ETicketUnclaimed _ TicketClaiming{..}) =
    let Entity ticket_id Ticket{..} = lookupErr "snowdriftEventToFeedEntry: comment id not present in ticket map" ticketClaimingTicket ticket_map
        ticket_str = case ticket_id of
            Key (PersistInt64 tid) -> T.pack $ show tid
            Key _ -> "<malformed id>"

     in Just $ FeedEntry
            { feedEntryLink    = CommentDirectLinkR ticketClaimingTicket
            , feedEntryUpdated = ticketClaimingTs
            , feedEntryTitle   = T.unwords
                [ T.snoc project_handle ':'
                , "ticket available:"
                , T.concat [ "SD-", ticket_str, ":" ]
                , ticketName
                ]
            , feedEntryContent = [hamlet| |] render
            }

snowdriftEventToFeedEntry render project_handle _ _ _ _ (EWikiPage _ wiki_page) =
    let target = wikiPageTarget wiki_page
     in Just $ FeedEntry
            { feedEntryLink    = WikiR project_handle $ wikiPageTarget wiki_page
            , feedEntryUpdated = wikiPageCreatedTs wiki_page
            , feedEntryTitle   = T.unwords [ T.snoc project_handle ':', "new wiki page", "\"" <> target <> "\"" ]
            , feedEntryContent = [hamlet| |] render
            }

snowdriftEventToFeedEntry render project_handle user_map _ wiki_page_map _ (EWikiEdit wiki_edit_id wiki_edit) =
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

snowdriftEventToFeedEntry render project_handle _ _ _ _
        (EBlogPost _
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
snowdriftEventToFeedEntry _ _ _ _ _ _ (ENewPledge _ _)         = Nothing
snowdriftEventToFeedEntry _ _ _ _ _ _ (EUpdatedPledge _ _ _)   = Nothing
snowdriftEventToFeedEntry _ _ _ _ _ _ (EDeletedPledge _ _ _ _) = Nothing

-- Graveyard of event types we don't want to put on the feed.
-- Don't match-all here, we don't want to accidentally not consider something.

snowdriftEventToFeedEntry _ _ _ _ _ _ (ENotificationSent _ _) = Nothing
snowdriftEventToFeedEntry _ _ _ _ _ _ (ECommentPending _ _)   = Nothing
