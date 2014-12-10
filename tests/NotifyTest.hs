{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE LambdaCase        #-}

module NotifyTest (notifySpecs) where

import           TestImport                           hiding ((=.), update, notificationContent)
import           Model.Language
import           Model.Notification

import           Control.Applicative                  ((<$>))
import           Control.Concurrent                   (threadDelay)
import           Control.Monad                        (unless)
import           Database.Esqueleto                   hiding (get)
import           Database.Esqueleto.Internal.Language (From)
import           Data.Foldable                        (forM_)
import qualified Data.List                            as L
import           Data.Monoid                          ((<>))
import           Data.Text                            (Text)
import qualified Data.Text                            as T
import           Yesod.Default.Config                 (AppConfig (..), DefaultEnv (..))
import           Yesod.Markdown                       (unMarkdown, Markdown)
import           Yesod.Routes.Class

establish :: UserId -> YesodExample App ()
establish user_id = [marked|
    get200 $ UserR user_id

    withStatus 302 False $ request $ do
        addNonce
        setMethod "POST"
        setUrl $ UserEstEligibleR user_id
        byLabel "Reason" "testing"
|]

selectUserId :: Text -> SqlPersistM UserId
selectUserId ident
    = (\case []    -> error $ "user not found: " <> T.unpack ident
             [uid] -> unValue uid
             uids  -> error $ "ident " <> T.unpack ident <> " must be unique, "
                           <> "but it matches these user ids: "
                           <> (L.intercalate ", " $ map (show . unValue) uids))
  <$> (select $ from $ \u -> do
           where_ $ u ^. UserIdent ==. val ident
           return $ u ^. UserId)

userId :: NamedUser -> Example UserId
userId = testDB . selectUserId . username

acceptHonorPledge :: YesodExample App ()
acceptHonorPledge = [marked|
    withStatus 302 False $ request $ do
        setMethod "POST"
        setUrl HonorPledgeR
|]

updateNotifPref :: UserId -> NotificationType -> NotificationDelivery
                -> SqlPersistM ()
updateNotifPref user_id notif_type notif_deliv =
    update $ \unp -> do
        set unp [ UserNotificationPrefType     =. val notif_type
                , UserNotificationPrefDelivery =. val notif_deliv ]
        where_ $ unp ^. UserNotificationPrefUser ==. val user_id

notificationContent :: From query expr backend (expr (Entity Notification))
                    => KeyBackend SqlBackend User -> NotificationType
                    -> query (expr (Value Markdown))
notificationContent user_id notif_type =
    from $ \n -> do
        where_ $ n ^. NotificationTo   ==. val user_id
             &&. n ^. NotificationType ==. val notif_type
        return $ n ^. NotificationContent

-- 'forkEventHandler' sleeps for one second in between
-- runs, so some tests will fail without this delay.
withDelay :: MonadIO m => m a -> m a
withDelay action = liftIO (threadDelay 1000000) >> action

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM mb a = mb >>= (`unless` a)

hasNotif :: UserId -> NotificationType -> Text -> String -> Bool
         -> YesodExample App ()
hasNotif user_id notif_type text err with_delay =
    if with_delay then withDelay hasNotif' else hasNotif'
  where
    hasNotif' =
        unlessM (testDB $ fmap (any $ T.isInfixOf text . unMarkdown . unValue)
                        $ select $ notificationContent user_id notif_type)
                (error err)

rethreadComment :: Text -> Text -> YesodExample App ()
rethreadComment rethread_route parent_route = [marked|
    get200 rethread_route

    withStatus 302 True $ request $ do
        addNonce
        setMethod "POST"
        setUrl rethread_route
        byLabel "New Parent Url" parent_route
        byLabel "Reason" "testing"
        addPostParam "mode" "post"
|]

flagComment :: Text -> YesodExample App ()
flagComment route = [marked|
    get200 route

    withStatus 302 True $ request $ do
        addNonce
        setMethod "POST"
        setUrl route
        addPostParam "f1" "1"
        addPostParam "f2" ""
        addPostParam "mode" "post"
|]

editComment :: Text -> YesodExample App ()
editComment route = [marked|
    get200 route

    withStatus 302 True $ request $ do
        addNonce
        setMethod "POST"
        setUrl route
        byLabel "Edit" "testing"
        addPostParam "mode" "post"
|]

named_users :: [NamedUser]
named_users = [minBound .. maxBound]

notifySpecs :: AppConfig DefaultEnv a -> Spec
notifySpecs AppConfig {..} = do
    ydescribe "notifications" $ mapM_ testNotification [minBound .. maxBound]

  where
    snowdrift route = route "snowdrift" LangEn "about"
    absolute route = appRoot <> "/" <> route
    render = absolute . T.intercalate "/" . fst . renderRoute

    testNotification NotifEligEstablish =
        yit "notifies on establishment" $ [marked|
            forM_ (L.init named_users) $ \user -> do
                user_id <- userId user
                loginAs AdminUser
                establish user_id

                hasNotif user_id NotifEligEstablish (render HonorPledgeR)
                    "establishment notification not found" False
                loginAs user
                acceptHonorPledge
        |]

    testNotification NotifReply =
        yit "notifies on reply" $ [marked|
            loginAs Mary
            postComment (snowdrift NewWikiDiscussionR) $
                byLabel "New Topic" "root comment"

            mary_id <- userId Mary
            testDB $ updateNotifPref mary_id NotifReply NotifDeliverWebsite

            loginAs Bob
            (comment_id, True) <- getLatestCommentId
            postComment
                (snowdrift ReplyWikiCommentR comment_id) $
                    byLabel "Reply" "reply to the root comment"

            (reply_id, True) <- getLatestCommentId
            hasNotif mary_id NotifReply (render $ CommentDirectLinkR reply_id)
                "reply notification not found" True
        |]

    testNotification NotifWelcome =
        yit "sends the welcome message when a user is created" $ [marked|
            forM_ named_users $ \user -> do
                 user_id <- userId user
                 hasNotif user_id NotifWelcome "Thanks for registering!"
                     "welcome notification not found" False
        |]

    -- XXX: Not triggered anywhere.
    testNotification NotifBalanceLow = return ()

    testNotification NotifUnapprovedComment =
        yit "notifies when a comment needs to be approved" $ [marked|
            let unestablished_user = L.last named_users
            loginAs unestablished_user
            postComment (snowdrift NewWikiDiscussionR) $
                byLabel "New Topic" "unapproved comment"
            (comment_id, False) <- getLatestCommentId
            user_id <- userId unestablished_user
            hasNotif user_id NotifUnapprovedComment
                (render $ snowdrift WikiCommentR comment_id)
                "unapproved comment notification not found" True
        |]

    -- XXX: Not triggered anywhere.
    testNotification NotifApprovedComment = return ()

    testNotification NotifRethreadedComment =
        yit "notifies when a comment is rethreaded" $ [marked|
            loginAs Mary
            postComment (snowdrift NewWikiDiscussionR) $
                byLabel "New Topic" "parent comment"
            (parent_id, True) <- getLatestCommentId

            loginAs Bob
            bob_id <- userId Bob
            testDB $ updateNotifPref bob_id
                NotifRethreadedComment NotifDeliverWebsite
            postComment (snowdrift NewWikiDiscussionR) $
                byLabel "New Topic" "rethreaded comment"
            (comment_id, True) <- getLatestCommentId

            loginAs AdminUser
            rethreadComment
                (render $ snowdrift RethreadWikiCommentR comment_id)
                (render $ snowdrift WikiCommentR parent_id)

            hasNotif bob_id NotifRethreadedComment
                (render $ snowdrift WikiCommentR comment_id)
                "rethreaded comment notification not found" True
        |]

    -- XXX: TODO.
    testNotification NotifEditConflict = return ()

    testNotification NotifFlag =
        yit "notifies when a comment gets flagged" $ [marked|
            loginAs Mary
            postComment (snowdrift NewWikiDiscussionR) $
                byLabel "New Topic" "flagged comment"
            (comment_id, True) <- getLatestCommentId
            mary_id <- userId Mary
            testDB $ updateNotifPref mary_id NotifFlag NotifDeliverWebsite

            loginAs Bob
            flagComment $ render $ snowdrift FlagWikiCommentR comment_id

            hasNotif mary_id NotifFlag (render $ snowdrift EditWikiCommentR comment_id)
                "flagged comment notification not found" False
        |]

    -- Relies on the 'NotifFlag' test.
    testNotification NotifFlagRepost =
        yit "notifies when a flagged comment gets reposted" $ [marked|
            bob_id <- userId Bob
            testDB $ updateNotifPref bob_id NotifFlagRepost NotifDeliverWebsite

            loginAs Mary
            (comment_id, True) <- getLatestCommentId
            editComment $ render $ snowdrift EditWikiCommentR comment_id

            hasNotif bob_id NotifFlagRepost
                (render $ snowdrift WikiCommentR comment_id)
                "flagged comment reposted notification not found" False
        |]

    -- XXX: TODO.
    testNotification NotifTicketClaimed   = return ()
    testNotification NotifTicketUnclaimed = return ()
    testNotification NotifWikiEdit        = return ()
    testNotification NotifWikiPage        = return ()
    testNotification NotifBlogPost        = return ()
    testNotification NotifNewPledge       = return ()
    testNotification NotifUpdatedPledge   = return ()
    testNotification NotifDeletedPledge   = return ()
