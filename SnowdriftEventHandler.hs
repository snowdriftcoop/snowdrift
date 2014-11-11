module SnowdriftEventHandler
    ( snowdriftEventHandlers
    ) where

import Import

import Model.Comment
import Model.Discussion
import Model.Notification
import Model.Project
import Model.User
import Model.Utils

import qualified Data.Foldable        as F
import           Data.Maybe           (fromJust)
import qualified Database.Persist
import           Yesod.Default.Config (AppConfig (..), DefaultEnv (..))
import           Yesod.Markdown

-- Add more event handlers here.
snowdriftEventHandlers :: AppConfig DefaultEnv Extra
                       -> [SnowdriftEvent -> Daemon ()]
snowdriftEventHandlers conf =
    [ notificationEventHandler conf
    , eventInserterHandler
    ]

-- | Handler in charge of sending Notifications to interested parties.
notificationEventHandler :: AppConfig DefaultEnv Extra
                         -> SnowdriftEvent -> Daemon ()
-- Notify the comment's parent's poster that their comment has been replied to (per their preferences).
notificationEventHandler AppConfig{..} (ECommentPosted comment_id comment) = case commentParent comment of
    Nothing -> return ()
    Just parent_comment_id -> do
        parent_comment_route <- routeToText $ CommentDirectLinkR parent_comment_id
        reply_comment_route  <- routeToText $ CommentDirectLinkR comment_id
        runSDB $ do
            parent_user_id <- commentUser <$> Database.Persist.getJust parent_comment_id
            sendPreferredNotificationDB parent_user_id NotifReply Nothing Nothing $
                mconcat [ "Someone replied to [your comment]("
                        , Markdown $ appRoot <> parent_comment_route
                        , ")! You can view the reply [here]("
                        , Markdown $ appRoot <> reply_comment_route
                        , "). *You can filter these notifications by " <>
                          "adjusting the settings in your profile.*"
                        ]

-- Notify all moderators of the project the comment was posted on.
-- Also notify the comment poster.
notificationEventHandler AppConfig{..} (ECommentPending comment_id comment) = runSDB $ do
    route_text <- lift (makeCommentRouteDB comment_id >>= lift . routeToText . fromJust)

    sendPreferredNotificationDB (commentUser comment) NotifUnapprovedComment Nothing Nothing $ mconcat
        [ "Your [comment]("
        , Markdown route_text
        , ") now awaits moderator approval."
        , "<br><br>"
        , "When a moderator acknowledges you as a legitimate user "
        , "(such as after you have posted a few meaningful comments), "
        , "you will become eligible for 'establishment'. "
        , "Established users can post without moderation."
        ]

    discussion <- lift $ fetchDiscussionDB $ commentDiscussion comment

    let projectComment (Entity project_id project) = do
            let content = mconcat
                  [ "An unapproved comment has been posted on a "
                  , Markdown (projectName project)
                  , " page. Please view it [here]("
                  , Markdown $ appRoot <> route_text
                  , ")."
                  ]

            mods <- lift $ fetchProjectModeratorsDB project_id
            F.forM_ mods $ \ user_id -> sendPreferredNotificationDB user_id NotifUnapprovedComment
                Nothing (Just comment_id) content

    case discussion of
        DiscussionOnProject  project                 -> projectComment project
        DiscussionOnWikiPage (Entity _ WikiPage{..}) -> projectComment =<< Entity wikiPageProject <$> getJust wikiPageProject
        DiscussionOnUser _ -> error ""

notificationEventHandler AppConfig{..} (ECommentApproved comment_id comment) = runSDB $ do
    route_text <- lift (makeCommentRouteDB comment_id >>= lift . routeToText . fromJust)
    sendPreferredNotificationDB (commentUser comment) NotifApprovedComment Nothing Nothing $ mconcat
        [ "Your [comment]("
        , Markdown route_text
        , ") has been approved."
        ]

-- Notify the rethreadee his/her comment has been rethreaded.
notificationEventHandler AppConfig{..} (ECommentRethreaded _ Rethread{..}) = do
    (comment, Just old_route, Just new_route) <- runDB $ (,,)
        <$> getJust rethreadOldComment
        <*> makeCommentRouteDB rethreadOldComment
        <*> makeCommentRouteDB rethreadNewComment

    rendered_old_route <- routeToText old_route
    rendered_new_route <- routeToText new_route

    let content = mconcat
          [ "One of your comments has been rethreaded from ~~"
          , Markdown $ appRoot <> rendered_old_route
          , "~~ to ["
          , Markdown $ appRoot <> rendered_new_route
          , "]("
          , Markdown $ appRoot <> rendered_new_route
          , "): "
          , Markdown rethreadReason
          ]

    runSDB $ sendPreferredNotificationDB (commentUser comment)
        NotifRethreadedComment Nothing Nothing content

notificationEventHandler _ (ECommentClosed _ _)     = return ()

-- TODO: Send notification to anyone watching thread
notificationEventHandler _ (ETicketClaimed _)       = return ()
notificationEventHandler _ (ETicketUnclaimed _ _)   = return ()

notificationEventHandler _ (ENotificationSent _ _)  = return ()
notificationEventHandler _ (EWikiEdit _ _)          = return ()
notificationEventHandler _ (EWikiPage _ _)          = return ()
notificationEventHandler _ (EBlogPost _ _)          = return ()
notificationEventHandler _ (ENewPledge _ _)         = return ()
notificationEventHandler _ (EUpdatedPledge _ _ _)   = return ()
notificationEventHandler _ (EDeletedPledge _ _ _ _) = return ()

-- | Handler in charge of inserting events (stripped down) into a separate table for each type.
eventInserterHandler :: SnowdriftEvent -> Daemon ()
-- If an unapproved comment is sent as an ECommentPosted event, bad things will happen (fromJust).
eventInserterHandler (ECommentPosted comment_id Comment{..})                         = runDB (insert_ (EventCommentPosted (fromJust commentApprovedTs) comment_id))
eventInserterHandler (ECommentPending comment_id Comment{..})                        = runDB (insert_ (EventCommentPending commentCreatedTs comment_id))
eventInserterHandler (ECommentRethreaded rethread_id Rethread{..})                   = runDB (insert_ (EventCommentRethreaded rethreadTs rethread_id))
eventInserterHandler (ECommentClosed comment_closing_id CommentClosing{..})          = runDB (insert_ (EventCommentClosing commentClosingTs comment_closing_id))
eventInserterHandler (ETicketClaimed (Left (ticket_claiming_id, TicketClaiming{..})))
                        = runDB (insert_ (EventTicketClaimed ticketClaimingTs (Just ticket_claiming_id) Nothing))
eventInserterHandler (ETicketClaimed (Right (ticket_old_claiming_id, TicketOldClaiming{..})))
                        = runDB (insert_ (EventTicketClaimed ticketOldClaimingClaimTs Nothing (Just ticket_old_claiming_id)))

eventInserterHandler (ETicketUnclaimed ticket_old_claiming_id TicketOldClaiming{..}) = runDB (insert_ (EventTicketUnclaimed ticketOldClaimingReleasedTs ticket_old_claiming_id))

eventInserterHandler (ENotificationSent notif_id Notification{..})                   = runDB (insert_ (EventNotificationSent notificationCreatedTs notif_id))
eventInserterHandler (EWikiPage wiki_page_id WikiPage{..})                           = runDB (insert_ (EventWikiPage wikiPageCreatedTs wiki_page_id))
eventInserterHandler (EWikiEdit wiki_edit_id WikiEdit{..})                           = runDB (insert_ (EventWikiEdit wikiEditTs wiki_edit_id))
eventInserterHandler (ENewPledge shares_pledged_id SharesPledged{..})                = runDB (insert_ (EventNewPledge sharesPledgedTs shares_pledged_id))
eventInserterHandler (EUpdatedPledge old_shares shares_pledged_id SharesPledged{..}) = runDB (insert_ (EventUpdatedPledge sharesPledgedTs old_shares shares_pledged_id))
eventInserterHandler (EDeletedPledge ts user_id project_id shares)                   = runDB (insert_ (EventDeletedPledge ts user_id project_id shares))
eventInserterHandler (EBlogPost post_id BlogPost{..})                                = runDB (insert_ (EventBlogPost blogPostTs post_id))

-- We don't have a table for ECommentApproved, because ECommentPosted is fired at the same time.
eventInserterHandler (ECommentApproved _ _) = return ()
