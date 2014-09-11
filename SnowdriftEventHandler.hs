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

-- Given a CommentId, render its permalink route as Text, inside an SDB action.
makeCommentRouteTextSDB :: CommentId -> SDB Text
makeCommentRouteTextSDB comment_id = lift (makeCommentRouteDB comment_id >>= lift . routeToText . fromJust)

--------------------------------------------------------------------------------

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
        when (isJust delivery) $ runSDB $ do
            parent_comment_route <- makeCommentRouteTextSDB parent_comment_id
            reply_comment_route  <- makeCommentRouteTextSDB comment_id

            let content = mconcat
                  [ "Someone replied to [your comment]("
                  , Markdown parent_comment_route
                  , ")! You can view the reply [here]("
                  , Markdown reply_comment_route
                  , ")."
                  , ""
                  , "*You can filter these notifications by adjusting the settings in your profile.*"
                  ]

            sendNotificationDB_ NotifReply parent_user_id Nothing content

-- Notify all moderators of the project the comment was posted on.
-- Also notify the comment poster.
notificationEventHandler (ECommentPending comment_id comment) = runSDB $ do
    route_text <- makeCommentRouteTextSDB comment_id

    sendNotificationDB_ NotifUnapprovedComment (commentUser comment) Nothing $ mconcat
        [ "Your [comment]("
        , Markdown route_text
        , ") now awaits moderator approval."
        , "<br><br>"
        , "When a moderator acknowledges you as a legitimate user "
        , "(such as after you have posted a few meaningful comments), "
        , "you will become eligible for 'establishment'. "
        , "Established users can post without moderation."
        ]

    (Entity project_id project) <- lift (fetchDiscussionDB (commentDiscussion comment)) >>= \case
        DiscussionOnProject  project                 -> return project
        DiscussionOnWikiPage (Entity _ WikiPage{..}) -> Entity wikiPageProject <$> getJust wikiPageProject

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

notificationEventHandler (ECommentApproved comment_id comment) = runSDB $ do
    route_text <- makeCommentRouteTextSDB comment_id
    sendNotificationDB_ NotifApprovedComment (commentUser comment) Nothing $ mconcat
        [ "Your [comment]("
        , Markdown route_text
        , ") has been approved."
        ]

-- Notify the rethreadee his/her comment has been rethreaded.
notificationEventHandler (ECommentRethreaded _ Rethread{..}) = runSDB $ do
    comment <- lift (getJust rethreadOldComment)
    old_route <- makeCommentRouteTextSDB rethreadOldComment
    new_route <- makeCommentRouteTextSDB rethreadNewComment

    let content = mconcat
          [ "One of your comments has been rethreaded from ~~"
          , Markdown old_route
          , "~~ to ["
          , Markdown new_route
          , "]("
          , Markdown new_route
          , "): "
          , Markdown rethreadReason
          ]

    sendNotificationDB_ NotifRethreadedComment (commentUser comment) Nothing content

notificationEventHandler (ENotificationSent _ _)  = return ()
notificationEventHandler (EWikiEdit _ _)          = return ()
notificationEventHandler (EWikiPage _ _)          = return ()
notificationEventHandler (ENewPledge _ _)         = return ()
notificationEventHandler (EUpdatedPledge _ _ _)   = return ()
notificationEventHandler (EDeletedPledge _ _ _ _) = return ()

-- | Handler in charge of inserting events (stripped down) into a separate table for each type.
eventInserterHandler :: SnowdriftEvent -> Daemon ()
-- If an unapproved comment is sent as an ECommentPosted event, bad things will happen (fromJust).
eventInserterHandler (ECommentPosted comment_id Comment{..})                         = runDB (insert_ (EventCommentPosted (fromJust commentApprovedTs) comment_id))
eventInserterHandler (ECommentPending comment_id Comment{..})                        = runDB (insert_ (EventCommentPending commentCreatedTs comment_id))
eventInserterHandler (ECommentRethreaded rethread_id Rethread{..})                   = runDB (insert_ (EventCommentRethreaded rethreadTs rethread_id))
eventInserterHandler (ENotificationSent notif_id Notification{..})                   = runDB (insert_ (EventNotificationSent notificationCreatedTs notif_id))
eventInserterHandler (EWikiPage wiki_page_id WikiPage{..})                           = runDB (insert_ (EventWikiPage wikiPageCreatedTs wiki_page_id))
eventInserterHandler (EWikiEdit wiki_edit_id WikiEdit{..})                           = runDB (insert_ (EventWikiEdit wikiEditTs wiki_edit_id))
eventInserterHandler (ENewPledge shares_pledged_id SharesPledged{..})                = runDB (insert_ (EventNewPledge sharesPledgedTs shares_pledged_id))
eventInserterHandler (EUpdatedPledge old_shares shares_pledged_id SharesPledged{..}) = runDB (insert_ (EventUpdatedPledge sharesPledgedTs old_shares shares_pledged_id))
eventInserterHandler (EDeletedPledge ts user_id project_id shares)                   = runDB (insert_ (EventDeletedPledge ts user_id project_id shares))
-- We don't have a table for ECommentApproved, because ECommentPosted is fired at the same time.
eventInserterHandler (ECommentApproved _ _) = return ()
