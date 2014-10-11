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

import           Data.Maybe           (fromJust)
import qualified Database.Persist
import           Yesod.Markdown

-- Add more event handlers here.
snowdriftEventHandlers :: [SnowdriftEvent -> Daemon ()]
snowdriftEventHandlers =
    [ notificationEventHandler
    , eventInserterHandler
    ]

-- | Handler in charge of sending Notifications to interested parties.
notificationEventHandler :: SnowdriftEvent -> Daemon ()
-- Notify the comment's parent's poster that their comment has been replied to (per their preferences).
notificationEventHandler (ECommentPosted comment_id comment) = case commentParent comment of
    Nothing -> return ()
    Just parent_comment_id -> do
        (parent_user_id, delivery) <- runDB $ do
            parent_user_id <- commentUser <$> Database.Persist.getJust parent_comment_id
            delivery <- fetchUserNotificationPrefDB parent_user_id NotifReply
            return (parent_user_id, delivery)
        -- Any non-Nothing delivery implies an internal Notification should be sent.
        when (isJust delivery) $ do
            parent_comment_route <- routeToText (CommentDirectLinkR parent_comment_id)
            reply_comment_route  <- routeToText (CommentDirectLinkR comment_id)

            let content = mconcat
                  [ "Someone replied to [your comment]("
                  , Markdown parent_comment_route
                  , ")! You can view the reply [here]("
                  , Markdown reply_comment_route
                  , ")."
                  , ""
                  , "*You can filter these notifications by adjusting the settings in your profile.*"
                  ]
            runSDB (sendNotificationDB_ NotifReply parent_user_id Nothing content)

-- Notify all moderators of the project the comment was posted on.
notificationEventHandler (ECommentPending comment_id comment) = do
    runSDB $ do
        (Entity project_id project) <- lift (fetchDiscussionDB (commentDiscussion comment)) >>= \case
            DiscussionOnProject  project                 -> return project
            DiscussionOnWikiPage (Entity _ WikiPage{..}) -> Entity wikiPageProject <$> getJust wikiPageProject

        route_text <- (lift . lift) (routeToText (CommentDirectLinkR comment_id)) -- TODO(mitchell): don't use direct link?
        let content = mconcat
              [ "An unapproved comment has been posted on a "
              , Markdown (projectName project)
              , " page. Please view it [here]("
              , Markdown route_text
              , ")."
              ]

        lift (fetchProjectModeratorsDB project_id) >>=
            -- Send the notification, and record the fact that we send it (so we can
            -- later delete it, when the comment is approved).
            mapM_ (\user_id -> sendNotificationDB NotifUnapprovedComment user_id Nothing content
                                 >>= insert_ . UnapprovedCommentNotification comment_id)

-- Notify the rethreadee his/her comment has been rethreaded.
notificationEventHandler (ECommentRethreaded _ Rethread{..}) = do
    (comment, Just old_route, Just new_route) <- runDB $ (,,)
        <$> getJust rethreadOldComment
        <*> makeCommentRouteDB rethreadOldComment
        <*> makeCommentRouteDB rethreadNewComment

    rendered_old_route <- routeToText old_route
    rendered_new_route <- routeToText new_route

    let content = mconcat
          [ "One of your comments has been rethreaded from ~~"
          , Markdown rendered_old_route
          , "~~ to ["
          , Markdown rendered_new_route
          , "]("
          , Markdown rendered_new_route
          , "): "
          , Markdown rethreadReason
          ]

    runSDB (sendNotificationDB_ NotifRethreadedComment (commentUser comment) Nothing content)

notificationEventHandler (ECommentClosed _ _)     = return ()

-- TODO: Send notification to anyone watching thread
notificationEventHandler (ETicketClaimed _ _)     = return ()
notificationEventHandler (ETicketUnclaimed _ _)     = return ()

notificationEventHandler (ENotificationSent _ _)  = return ()
notificationEventHandler (EWikiEdit _ _)          = return ()
notificationEventHandler (EWikiPage _ _)          = return ()
notificationEventHandler (EBlogPost _ _)          = return ()
notificationEventHandler (ENewPledge _ _)         = return ()
notificationEventHandler (EUpdatedPledge _ _ _)   = return ()
notificationEventHandler (EDeletedPledge _ _ _ _) = return ()

-- | Handler in charge of inserting events (stripped down) into a separate table for each type.
eventInserterHandler :: SnowdriftEvent -> Daemon ()
-- If an unapproved comment is sent as an ECommentPosted event, bad things will happen (fromJust).
eventInserterHandler (ECommentPosted comment_id Comment{..})                         = runDB (insert_ (EventCommentPosted (fromJust commentApprovedTs) comment_id))
eventInserterHandler (ECommentPending comment_id Comment{..})                        = runDB (insert_ (EventCommentPending commentCreatedTs comment_id))
eventInserterHandler (ECommentRethreaded rethread_id Rethread{..})                   = runDB (insert_ (EventCommentRethreaded rethreadTs rethread_id))
eventInserterHandler (ECommentClosed comment_closing_id CommentClosing{..})          = runDB (insert_ (EventCommentClosing commentClosingTs comment_closing_id))
eventInserterHandler (ETicketClaimed ticket_claiming_id TicketClaiming{..})          = runDB (insert_ (EventTicketClaimed ticketClaimingTs ticket_claiming_id))

eventInserterHandler (ETicketUnclaimed ticket_claiming_id TicketClaiming{..})        =
    let released = fromMaybe (error "TicketUnclaimed event for TicketClaiming without ReleasedTs") ticketClaimingReleasedTs
     in runDB (insert_ (EventTicketUnclaimed released ticket_claiming_id))

eventInserterHandler (ENotificationSent notif_id Notification{..})                   = runDB (insert_ (EventNotificationSent notificationCreatedTs notif_id))
eventInserterHandler (EWikiPage wiki_page_id WikiPage{..})                           = runDB (insert_ (EventWikiPage wikiPageCreatedTs wiki_page_id))
eventInserterHandler (EWikiEdit wiki_edit_id WikiEdit{..})                           = runDB (insert_ (EventWikiEdit wikiEditTs wiki_edit_id))
eventInserterHandler (ENewPledge shares_pledged_id SharesPledged{..})                = runDB (insert_ (EventNewPledge sharesPledgedTs shares_pledged_id))
eventInserterHandler (EUpdatedPledge old_shares shares_pledged_id SharesPledged{..}) = runDB (insert_ (EventUpdatedPledge sharesPledgedTs old_shares shares_pledged_id))
eventInserterHandler (EDeletedPledge ts user_id project_id shares)                   = runDB (insert_ (EventDeletedPledge ts user_id project_id shares))
eventInserterHandler (EBlogPost post_id BlogPost{..})                                = runDB (insert_ (EventBlogPost blogPostTs post_id))
