module SnowdriftEventHandler
    ( snowdriftEventHandlers
    ) where

import Import

import Data.Maybe (fromJust)
import Yesod.Default.Config (AppConfig (..), DefaultEnv (..))
import qualified Data.Foldable as F
import qualified Data.Text as T
import qualified Database.Persist

import Model.Comment
import Model.Currency
import Model.Discussion
import Model.Project
import Model.User
import Model.Utils

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
            parent_user_id <- commentUser <$> lift (Database.Persist.getJust parent_comment_id)
            sendPreferredUserNotificationDB
                (Just $ NotificationSender $ commentUser comment)
                (NotificationReceiver parent_user_id)
                NotifReply
                Nothing
                (mconcat
                    [ "Someone replied to [your comment]("
                    , Markdown $ appRoot <> parent_comment_route
                    , ")! You can view the reply [here]("
                    , Markdown $ appRoot <> reply_comment_route
                    , "). *You can filter these notifications by " <>
                    "adjusting the settings in your profile.*"
                    ])

-- Notify all moderators of the project the comment was posted on.
-- Also notify the comment poster.
notificationEventHandler AppConfig{..} (ECommentPending comment_id comment) = runSDB $ do
    route_text <- lift (makeCommentRouteDB [LangEn] comment_id >>= lift . routeToText . fromJust)
    let user_id = commentUser comment
    sendPreferredUserNotificationDB
        Nothing (NotificationReceiver user_id)
        NotifUnapprovedComment
        Nothing
        (mconcat
            [ "Your [comment]("
            , Markdown $ appRoot <> route_text
            , ") now awaits moderator approval."
            , "<br><br>"
            , "When a moderator acknowledges you as a legitimate user "
            , "(such as after you have posted a few meaningful comments), "
            , "you will become eligible for 'establishment'. "
            , "Established users can post without moderation."
            ])

    discussion <- lift $ fetchDiscussionDB [LangEn] $ commentDiscussion comment

    let projectComment (Entity project_id project) = do
            let content = mconcat
                  [ "An unapproved comment has been posted on a "
                  , Markdown (projectName project)
                  , " page. Please view it [here]("
                  , Markdown $ appRoot <> route_text
                  , ")."
                  ]

            mods <- lift $ fetchProjectModeratorsDB project_id
            F.forM_ mods $ \mod_id ->
                sendPreferredUserNotificationDB
                    (Just $ NotificationSender user_id)
                    (NotificationReceiver mod_id)
                    NotifUnapprovedComment
                    (Just comment_id)
                    content

    case discussion of
        DiscussionOnProject project                     -> projectComment project
        DiscussionOnWikiPage (Entity _ WikiTarget{..})  -> projectComment =<< Entity wikiTargetProject <$> lift (getJust wikiTargetProject)
        DiscussionOnUser _                              -> error ""
        DiscussionOnBlogPost (Entity _ BlogPost{..})    -> projectComment =<< Entity blogPostProject <$> lift (getJust blogPostProject)

notificationEventHandler AppConfig{..} (ECommentApproved comment_id comment) = runSDB $ do
    route_text <- lift (makeCommentRouteDB [LangEn] comment_id >>= lift . routeToText . fromJust)
    sendPreferredUserNotificationDB
        (fmap NotificationSender $ commentApprovedBy comment)
        (NotificationReceiver $ commentUser comment)
        NotifApprovedComment
        Nothing
        (mconcat
            [ "Your [comment]("
            , Markdown $ appRoot <> route_text
            , ") has been approved."
            ])

-- Notify the rethreadee his/her comment has been rethreaded.
notificationEventHandler AppConfig{..} (ECommentRethreaded _ Rethread{..}) = do
    (comment, Just old_route, Just new_route) <- runDB $ (,,)
        <$> getJust rethreadOldComment
        <*> makeCommentRouteDB [LangEn] rethreadOldComment
        <*> makeCommentRouteDB [LangEn] rethreadNewComment

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

    runSDB $ sendPreferredUserNotificationDB
        (Just $ NotificationSender rethreadModerator)
        (NotificationReceiver $ commentUser comment)
        NotifRethreadedComment
        Nothing
        content

notificationEventHandler _ (ECommentClosed _ _)     = return ()

notificationEventHandler _ (EUserNotificationSent _ _)    = return ()
notificationEventHandler _ (EProjectNotificationSent _ _) = return ()

-- TODO: Send notification to anyone watching thread
notificationEventHandler _ (ETicketClaimed _)       = return ()
notificationEventHandler _ (ETicketUnclaimed _ _)   = return ()

notificationEventHandler AppConfig{..} (EWikiEdit wiki_edit_id wiki_edit wiki_target) =
    runSDB $ handleWatched
        (Just $ NotificationSender $ wikiEditUser wiki_edit)
        appRoot (wikiTargetProject wiki_target)
        (\project_handle ->
            WikiEditR
                project_handle
                (wikiTargetLanguage wiki_target)
                (wikiTargetTarget wiki_target)
                wiki_edit_id)
        NotifWikiEdit
        (\route -> "Wiki page [edited](" <> route <> ")")

notificationEventHandler AppConfig{..} (EWikiPage _ wiki_page wiki_target) =
    runSDB $ handleWatched
        (Just $ NotificationSender $ wikiPageUser wiki_page)
        appRoot (wikiPageProject wiki_page)
        (\project_handle ->
            WikiR
                project_handle
                (wikiTargetLanguage wiki_target)
                (wikiTargetTarget wiki_target))
        NotifWikiPage
        (\route -> "New [wiki page](" <> route <> ")")

notificationEventHandler AppConfig{..} (EBlogPost _ blog_post) =
    runSDB $ handleWatched
        (Just $ NotificationSender $ blogPostUser blog_post)
        appRoot
        (blogPostProject blog_post)
        (\project_handle -> BlogPostR project_handle $ blogPostHandle blog_post)
        NotifBlogPost
        (\route -> "New [blog post](" <> route <> ")")

notificationEventHandler AppConfig{..} (ENewPledge _ shares_pledged) = runSDB $ do
    users <- lift $ selectList [UserId <-. [sharesPledgedUser shares_pledged]] []
    let shares = sharesPledgedShares shares_pledged
    forM_ users $ \user_entity ->
        handleWatched
            (Just $ NotificationSender $ entityKey user_entity)
            appRoot
            (sharesPledgedProject shares_pledged)
            ProjectPatronsR
            NotifNewPledge
            (\route -> T.concat
                 [ userDisplayName user_entity
                 , " pledged ["
                 , T.pack $ show $ millMilray shares
                 , "](", route, ")"
                 ])

notificationEventHandler AppConfig{..} (EUpdatedPledge old_shares _ shares_pledged) = runSDB $ do
    users <- lift $ selectList [UserId <-. [sharesPledgedUser shares_pledged]] []
    let new_shares = sharesPledgedShares shares_pledged
        delta      = abs $ old_shares - new_shares
    forM_ users $ \user_entity ->
        handleWatched
            (Just $ NotificationSender $ entityKey user_entity)
            appRoot
            (sharesPledgedProject shares_pledged)
            ProjectPatronsR
            NotifUpdatedPledge
            (\route -> T.concat
                 [ userDisplayName user_entity
                 , if old_shares > new_shares then " dropped " else " added "
                 , T.pack $ show $ millMilray delta
                 , ", changing their total to ["
                 , T.pack $ show $ millMilray new_shares
                 , "](", route, ")"
                 ])

notificationEventHandler AppConfig{..} (EDeletedPledge _ user_id project_id _) = runSDB $ do
    user <- lift (get user_id)
    sequence_ $ user >>= \u -> pure $
        handleWatched
            (Just $ NotificationSender user_id)
            appRoot
            project_id
            ProjectPatronsR
            NotifDeletedPledge
            (\route -> userDisplayName (Entity user_id u)
                   <> " is no longer supporting the [project](" <> route <> ")")

notificationEventHandler AppConfig{..} (EVolunteerApp _ user_id project_id app_id) =
    runSDB $
        lift (get project_id) >>= \case
            Just project ->
                handleWatched
                    (Just (NotificationSender user_id))
                    appRoot
                    project_id
                    (`ApplicationR` app_id)
                    NotifVolunteerApp
                    (\route -> mconcat
                        [ "New [volunteer application]("
                        , route
                        , ") submitted for "
                        , projectName project
                        , "."
                        ])
            -- Nothing will match if the project is deleted from the database
            -- by the time this event is handled.
            Nothing -> pure ()

handleWatched
    :: Maybe NotificationSender
    -> Text
    -> ProjectId
    -> (Text -> Route App)
    -> ProjectNotificationType
    -> (Text -> Text)
    -> SDB ()
handleWatched mnotif_sender appRoot project_id mkRoute notif_type mkMsg = do
    projects <- lift $ fetchProjectDB project_id
    forM_ projects $ \(Entity _ project) -> do
        route <- lift $ lift $ routeToText $ mkRoute $ projectHandle project
        user_ids <- lift $
            fetchUsersByProjectNotifPrefDB notif_type project_id
        forM_ user_ids $ \user_id -> do
            is_watching <- lift $ userIsWatchingProjectDB user_id project_id
            when is_watching $
                sendPreferredProjectNotificationDB
                    mnotif_sender
                    (NotificationReceiver user_id)
                    notif_type
                    project_id
                    (Markdown $ mkMsg $ appRoot <> route)

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

eventInserterHandler (EUserNotificationSent notif_id UserNotification{..})           = runDB (insert_ (EventUserNotificationSent userNotificationCreatedTs notif_id))
eventInserterHandler (EProjectNotificationSent notif_id ProjectNotification{..})     = runDB (insert_ (EventProjectNotificationSent projectNotificationCreatedTs notif_id))

eventInserterHandler (EWikiPage wiki_page_id WikiPage{..} _)                         = runDB (insert_ (EventWikiPage wikiPageCreatedTs wiki_page_id))
eventInserterHandler (EWikiEdit wiki_edit_id WikiEdit{..} _)                         = runDB (insert_ (EventWikiEdit wikiEditTs wiki_edit_id))
eventInserterHandler (ENewPledge shares_pledged_id SharesPledged{..})                = runDB (insert_ (EventNewPledge sharesPledgedTs shares_pledged_id))
eventInserterHandler (EUpdatedPledge old_shares shares_pledged_id SharesPledged{..}) = runDB (insert_ (EventUpdatedPledge sharesPledgedTs old_shares shares_pledged_id))
eventInserterHandler (EDeletedPledge ts user_id project_id shares)                   = runDB (insert_ (EventDeletedPledge ts user_id project_id shares))
eventInserterHandler (EBlogPost post_id BlogPost{..})                                = runDB (insert_ (EventBlogPost blogPostTs post_id))

-- We don't have a table for ECommentApproved, because ECommentPosted is fired at the same time.
eventInserterHandler (ECommentApproved _ _) = return ()

eventInserterHandler EVolunteerApp{} = return ()
