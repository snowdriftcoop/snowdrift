{-# LANGUAGE RecordWildCards #-}

module Model.SnowdriftEvent
    ( snowdriftEventNewestToOldest
    , snowdriftEventTime
    , snowdriftEventToFeedEntry
    ) where

import Import

import Model.User

import qualified Data.Map   as M
import qualified Data.Text  as T
import           Yesod.Feed (FeedEntry(..))

snowdriftEventNewestToOldest :: SnowdriftEvent -> SnowdriftEvent -> Ordering
snowdriftEventNewestToOldest x y  = compare (snowdriftEventTime y) (snowdriftEventTime x)

snowdriftEventTime :: SnowdriftEvent -> UTCTime
snowdriftEventTime (ECommentApproved _ Comment{..})                     = commentCreatedTs
snowdriftEventTime (ECommentPosted _ Comment{..})                       = fromMaybe commentCreatedTs commentApprovedTs
snowdriftEventTime (ECommentPending _ Comment{..})                      = commentCreatedTs
snowdriftEventTime (ECommentRethreaded _ Rethread{..})                  = rethreadTs
snowdriftEventTime (ECommentClosed _ CommentClosing{..})                = commentClosingTs
snowdriftEventTime (ETicketClaimed (Left (_,  TicketClaiming{..})))     = ticketClaimingTs
snowdriftEventTime (ETicketClaimed (Right (_,  TicketOldClaiming{..}))) = ticketOldClaimingClaimTs
snowdriftEventTime (ETicketUnclaimed _ TicketOldClaiming{..})           = ticketOldClaimingReleasedTs
snowdriftEventTime (ENotificationSent _ Notification{..})               = notificationCreatedTs
snowdriftEventTime (EWikiEdit _ WikiEdit{..} _)                         = wikiEditTs
snowdriftEventTime (EWikiPage _ WikiPage{..} _)                         = wikiPageCreatedTs
snowdriftEventTime (EBlogPost _ BlogPost{..})                           = blogPostTs
snowdriftEventTime (ENewPledge _ SharesPledged{..})                     = sharesPledgedTs
snowdriftEventTime (EUpdatedPledge _ _ SharesPledged{..})               = sharesPledgedTs
snowdriftEventTime (EDeletedPledge ts _ _ _)                            = ts

-- Eventually the html rendering here should be moved to the top level somewhere for sharing with notifications
snowdriftEventToFeedEntry
        :: (Route App -> Text)
        -> Text
        -- -> Prefetch
        -> Map UserId User
        -> Map DiscussionId DiscussionOn
        -> Map WikiPageId WikiTarget
        -> Map CommentId (Entity Ticket)
        -> Route App
        -> SnowdriftEvent
        -> Maybe (FeedEntry (Route App))

snowdriftEventToFeedEntry render project_handle user_map discussion_map _ _ url (ECommentPosted _ comment) =
    let user_id    = commentUser comment
        maybe_user = M.lookup user_id user_map
        username   = maybe "<unknown user>" (userDisplayName . Entity user_id) maybe_user
        discussion = case M.lookup (commentDiscussion comment) discussion_map of
            Nothing                                             -> "<unknown discussion>"
            Just (DiscussionOnProject _)                        -> "project discussion"
            Just (DiscussionOnWikiPage (Entity _ wiki_target))  -> "wiki discussion for \"" <> wikiTargetTarget wiki_target <> "\""
            Just (DiscussionOnUser user_entity)                 -> "user discussion for " <> userDisplayName user_entity
            Just (DiscussionOnBlogPost (Entity _ blog_post))    -> "discussion on blog post \"" <> blogPostTitle blog_post <> "\""

     in Just $ FeedEntry
            { feedEntryLink    = url
            , feedEntryUpdated = maybe (commentCreatedTs comment) id $ commentApprovedTs comment
            , feedEntryTitle   = T.unwords [ T.snoc project_handle ':', username, "posted a new comment on", discussion ]
            , feedEntryContent = [hamlet| |] render
            }

snowdriftEventToFeedEntry render project_handle _ _ _ _ url (ECommentRethreaded _ rethread) =
    Just $ FeedEntry
        { feedEntryLink    = url
        , feedEntryUpdated = rethreadTs rethread
        , feedEntryTitle   = T.unwords [ T.snoc project_handle ':', "comment(s) rethreaded" ]
        , feedEntryContent = [hamlet| |] render
        }

snowdriftEventToFeedEntry render project_handle user_map _ _ ticket_map url (ECommentClosed _ CommentClosing{..}) =
    let user_id    = commentClosingClosedBy
        maybe_user = M.lookup user_id user_map
        username   = maybe "<unknown user>" (userDisplayName . Entity user_id) maybe_user

        mk_feed_entry title = Just $ FeedEntry
            { feedEntryLink    = url
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

snowdriftEventToFeedEntry render project_handle user_map _ _ ticket_map url (ETicketClaimed (Left (_, TicketClaiming{..}))) =
    let user_id    = ticketClaimingUser
        maybe_user = M.lookup user_id user_map
        username   = maybe "<unknown user>" (userDisplayName . Entity user_id) maybe_user
        Entity ticket_id Ticket{..} = lookupErr "snowdriftEventToFeedEntry: comment id not present in ticket map" ticketClaimingTicket ticket_map
        ticket_str = case ticket_id of
            Key (PersistInt64 tid) -> T.pack $ show tid
            Key _ -> "<malformed id>"

     in Just $ FeedEntry
            { feedEntryLink    = url
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

snowdriftEventToFeedEntry render project_handle user_map _ _ ticket_map url (ETicketClaimed (Right (_, TicketOldClaiming{..}))) =
    let user_id    = ticketOldClaimingUser
        maybe_user = M.lookup user_id user_map
        username   = maybe "<unknown user>" (userDisplayName . Entity user_id) maybe_user
        Entity ticket_id Ticket{..} = lookupErr "snowdriftEventToFeedEntry: comment id not present in ticket map" ticketOldClaimingTicket ticket_map
        ticket_str = case ticket_id of
            Key (PersistInt64 tid) -> T.pack $ show tid
            Key _ -> "<malformed id>"

     in Just $ FeedEntry
            { feedEntryLink    = url
            , feedEntryUpdated = ticketOldClaimingClaimTs
            , feedEntryTitle   = T.unwords
                [ T.snoc project_handle ':'
                , "ticket claimed by"
                , T.snoc username ':'
                , T.concat [ "SD-", ticket_str, ":" ]
                , ticketName
                ]
            , feedEntryContent = [hamlet| |] render
            }

snowdriftEventToFeedEntry render project_handle _ _ _ ticket_map url (ETicketUnclaimed _ TicketOldClaiming{..}) =
    let Entity ticket_id Ticket{..} = lookupErr "snowdriftEventToFeedEntry: comment id not present in ticket map" ticketOldClaimingTicket ticket_map
        ticket_str = case ticket_id of
            Key (PersistInt64 tid) -> T.pack $ show tid
            Key _ -> "<malformed id>"

     in Just $ FeedEntry
            { feedEntryLink    = url
            , feedEntryUpdated = ticketOldClaimingReleasedTs
            , feedEntryTitle   = T.unwords
                [ T.snoc project_handle ':'
                , "ticket available:"
                , T.concat [ "SD-", ticket_str, ":" ]
                , ticketName
                ]
            , feedEntryContent = [hamlet| |] render
            }

snowdriftEventToFeedEntry render project_handle _ _ _ _ url (EWikiPage _ wiki_page wiki_target) =
    let target = wikiTargetTarget wiki_target
     in Just $ FeedEntry
            { feedEntryLink    = url
            , feedEntryUpdated = wikiPageCreatedTs wiki_page
            , feedEntryTitle   = T.unwords [ T.snoc project_handle ':', "new wiki page", "\"" <> target <> "\"" ]
            , feedEntryContent = [hamlet| |] render
            }

snowdriftEventToFeedEntry render project_handle user_map _ _ _ url (EWikiEdit _ wiki_edit wiki_target) =
    let target          = wikiTargetTarget wiki_target
        user_id         = wikiEditUser wiki_edit
        edit_language   = wikiEditLanguage wiki_edit
        maybe_user      = M.lookup user_id user_map
        username        = maybe "<unknown user>" (userDisplayName . Entity user_id) maybe_user

     in Just $ FeedEntry
            { feedEntryLink    = url
            , feedEntryUpdated = wikiEditTs wiki_edit
            , feedEntryTitle   = T.unwords
                [ T.snoc project_handle ':'
                , renderLanguage LangEn edit_language
                , "version of wiki page", "\"" <> target <> "\""
                , "edited by", username
                ] <> maybe "" (T.append ": ") (wikiEditComment wiki_edit)

            , feedEntryContent = [hamlet| |] render
            }

snowdriftEventToFeedEntry render project_handle _ _ _ _ url
        (EBlogPost _
            BlogPost
                { blogPostTs = ts
                , blogPostTitle = title
                }
        ) =
    Just $ FeedEntry
        { feedEntryLink    = url
        , feedEntryUpdated = ts
        , feedEntryTitle   = T.unwords [ T.snoc project_handle ':', "new blog post:", "\"" <> title <> "\"" ]
        , feedEntryContent = [hamlet| |] render
        }


-- We might want to show these, but I'm not sure.  Leaving them out now, at any rate.
snowdriftEventToFeedEntry _ _ _ _ _ _ _ (ENewPledge _ _)         = Nothing
snowdriftEventToFeedEntry _ _ _ _ _ _ _ (EUpdatedPledge _ _ _)   = Nothing
snowdriftEventToFeedEntry _ _ _ _ _ _ _ (EDeletedPledge _ _ _ _) = Nothing

-- Graveyard of event types we don't want to put on the feed.
-- Don't match-all here, we don't want to accidentally not consider something.

snowdriftEventToFeedEntry _ _ _ _ _ _ _ (ECommentApproved _ _)  = Nothing
snowdriftEventToFeedEntry _ _ _ _ _ _ _ (ECommentPending _ _)   = Nothing
snowdriftEventToFeedEntry _ _ _ _ _ _ _ (ENotificationSent _ _) = Nothing
