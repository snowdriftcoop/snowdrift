module SnowdriftEventHandler
    ( snowdriftEventHandlers
    ) where

import Import

import           Model.Discussion
import           Model.Notification
import           Model.Project
import           Model.User

import           Blaze.ByteString.Builder (toLazyByteString)
import           Control.Monad.Reader
import           Data.Maybe               (fromJust)
import qualified Data.Text.Lazy           as TL
import qualified Data.Text.Lazy.Encoding  as TLE
import qualified Database.Persist
import           Yesod                    (renderRoute)
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
            app <- ask
            let parent_comment_route = renderRoute' (CommentDirectLinkR parent_comment_id) app
                reply_comment_route  = renderRoute' (CommentDirectLinkR comment_id)        app

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
    app <- ask
    runSDB $ lift (fetchDiscussionProjectDB (commentDiscussion comment)) >>= \case
        Nothing -> return () -- Comment wasn't on a project, somehow? I guess do nothing.
        Just project_id -> do
            project <- getJust project_id

            let content = mconcat
                  [ "An unapproved comment has been posted on a "
                  , Markdown (projectName project)
                  , " page. Please view it [here]("
                  , Markdown (renderRoute' (CommentDirectLinkR comment_id) app)
                  , ")."
                  ]

            lift (fetchProjectModeratorsDB project_id) >>=
                -- Send the notification, and record the fact that we send it (so we can
                -- later delete it, when the comment is approved).
                mapM_ (\user_id -> sendNotificationDB NotifUnapprovedComment user_id Nothing content
                                     >>= insert_ . UnapprovedCommentNotification comment_id)
notificationEventHandler (ENotificationSent _ _)       = return ()
notificationEventHandler (EWikiEdit _ _)          = return ()
notificationEventHandler (EWikiPage _ _)          = return ()
notificationEventHandler (ENewPledge _ _)         = return ()
notificationEventHandler (EUpdatedPledge _ _ _)   = return ()
notificationEventHandler (EDeletedPledge _ _ _ _) = return ()

-- | Handler in charge of inserting events (stripped down) into a separate table for each type.
eventInserterHandler :: SnowdriftEvent -> Daemon ()
-- If an unapproved comment is sent as an ECommentPosted event, bad things will happen (fromJust).
eventInserterHandler (ECommentPosted comment_id Comment{..})                         = runDB (insert_ (EventCommentPosted (fromJust commentModeratedTs) comment_id))
eventInserterHandler (ECommentPending comment_id Comment{..})                        = runDB (insert_ (EventCommentPending commentCreatedTs comment_id))
eventInserterHandler (ENotificationSent notif_id Notification{..})                   = runDB (insert_ (EventNotificationSent notificationCreatedTs notif_id))
eventInserterHandler (EWikiPage wiki_page_id WikiPage{..})                           = runDB (insert_ (EventWikiPage wikiPageCreatedTs wiki_page_id))
eventInserterHandler (EWikiEdit wiki_edit_id WikiEdit{..})                           = runDB (insert_ (EventWikiEdit wikiEditTs wiki_edit_id))
eventInserterHandler (ENewPledge shares_pledged_id SharesPledged{..})                = runDB (insert_ (EventNewPledge sharesPledgedTs shares_pledged_id))
eventInserterHandler (EUpdatedPledge old_shares shares_pledged_id SharesPledged{..}) = runDB (insert_ (EventUpdatedPledge sharesPledgedTs old_shares shares_pledged_id))
eventInserterHandler (EDeletedPledge ts user_id project_id shares)                   = runDB (insert_ (EventDeletedPledge ts user_id project_id shares))

renderRoute' :: Route App -> App -> Text
renderRoute' route app =
    let (path_pieces, query_params) = renderRoute route
    -- converting a lazy ByteString to a strict Text... ridiculous!
    -- why does joinPath return a ByteString??
    in TL.toStrict $ TLE.decodeUtf8 $ toLazyByteString (joinPath app "" path_pieces query_params)
