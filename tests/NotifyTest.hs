{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE GADTs             #-}

module NotifyTest (notifySpecs) where

import           Import                               (Established(..))
import           TestImport                           hiding ((=.), update, notificationContent, (</>), Update)
import           Model.Language
import           Model.Notification

import           Control.Exception                    (bracket)
import           Control.Monad                        (void, unless)
import           Database.Esqueleto
import           Database.Esqueleto.Internal.Language (Update)
import           Data.Foldable                        (forM_)
import qualified Data.List                            as L
import           Data.Monoid                          ((<>))
import           Data.Text                            (Text)
import qualified Data.Text                            as Text
import qualified Data.Text.IO                         as Text
import           System.FilePath                      ((</>))
import           System.Process                       (spawnProcess, terminateProcess)
import           Yesod.Default.Config                 (AppConfig (..), DefaultEnv (..))

updateUser :: UserId -> [SqlExpr (Update User)] -> SqlPersistM ()
updateUser user_id xs =
    update $ \u -> do
        set u xs
        where_ $ u ^. UserId ==. val user_id

unestablish :: UserId -> SqlPersistM ()
unestablish user_id = do
    delete $ from $ \me ->
        where_ $ me ^. ManualEstablishmentEstablishedUser ==. val user_id
    updateUser user_id [UserEstablished =. val EstUnestablished]

addAndVerifyEmail :: UserId -> Text -> SqlPersistM ()
addAndVerifyEmail user_id email =
    updateUser user_id [ UserEmail =. val (Just email)
                       , UserEmail_verified =. val True ]

withEmailDaemon :: FilePath -> (FilePath -> IO a) -> IO ()
withEmailDaemon file action = do
    let prefix = "dist/build"
    withDelay $ bracket
        (spawnProcess
             (prefix </> "SnowdriftEmailDaemon/SnowdriftEmailDaemon")
             [ "--sendmail=" <> prefix </> "SnowdriftSendmail/SnowdriftSendmail"
             , "--sendmail-file=" <> file
             , "--db=testing"
             ])
        terminateProcess
        (const $ withDelay $ void $ action file)

hasUniqueEmailNotif :: FilePath -> Text -> IO Bool
hasUniqueEmailNotif file text = do
    contents <- Text.readFile file
    return $ Text.count text contents == 1

errUnlessUniqueEmailNotif :: FilePath -> Text -> IO ()
errUnlessUniqueEmailNotif file text = do
    has_notif <- hasUniqueEmailNotif file text
    unless has_notif $
        error $ "could not find " <> Text.unpack text <> " in " <> file

notifySpecs :: AppConfig DefaultEnv a -> FilePath -> Spec
notifySpecs AppConfig {..} file = do
    -- Note that since we rely on 'Bounded' here, the order of the
    -- 'NotificationType' value constructors is important for some of
    -- these tests.  For example, 'NotifWikiEdit' must not be tested
    -- prior to 'NotifWikiPage'.  Otherwise, there will be nothing to
    -- edit.
    ydescribe "notifications" $ mapM_ testNotification [minBound .. maxBound]

  where
    wiki_page      = "testing"
    shares, shares' :: Int
    shares         = 2
    shares'        = succ shares

    wiki_page_email = "testing-email"
    shares_email, shares_email' :: Int
    shares_email    = shares
    shares_email'   = succ shares'

    testNotification NotifEligEstablish = do
        yit "notifies on establishment" $ [marked|
            forM_ (L.init named_users) $ \user -> do
                user_id <- userId user
                loginAs AdminUser
                establish user_id

                hasNotif user_id NotifEligEstablish
                    (render appRoot $ HonorPledgeR)
                    "establishment notification not found" False
                loginAs user
                acceptHonorPledge
        |]

        yit "send an email when a user is eligible for establishment" $ [marked|
            mary_id <- userId Mary
            testDB $ unestablish mary_id
            testDB $ addAndVerifyEmail mary_id "mary@localhost"
            loginAs AdminUser
            establish mary_id
            liftIO $ withEmailDaemon file $ flip errUnlessUniqueEmailNotif
                "You are now eligible to become an *established* user"
            loginAs Mary
            acceptHonorPledge
        |]

    testNotification NotifReply = do
        yit "notifies on reply" $ [marked|
            loginAs Mary
            postComment (enRoute NewWikiDiscussionR "about") $
                byLabel "New Topic" "root comment"

            mary_id <- userId Mary
            testDB $ updateNotifPrefs mary_id Nothing NotifReply $
                singleton NotifDeliverWebsite

            loginAs Bob
            (comment_id, True) <- getLatestCommentId
            postComment
                (enRoute ReplyWikiCommentR "about" comment_id) $
                    byLabel "Reply" "reply to the root comment"

            (reply_id, True) <- getLatestCommentId
            hasNotif mary_id NotifReply
                (render appRoot $ CommentDirectLinkR reply_id)
                "reply notification not found" True
        |]

        yit "sends an email on reply" $ [marked|
            loginAs Mary
            postComment (enRoute NewWikiDiscussionR "about") $
                byLabel "New Topic" "root comment (email)"

            mary_id <- userId Mary
            testDB $ updateNotifPrefs mary_id Nothing NotifReply $
                singleton NotifDeliverEmail

            loginAs Bob
            (comment_id, True) <- getLatestCommentId
            postComment
                (enRoute ReplyWikiCommentR "about" comment_id) $
                    byLabel "Reply" "reply to the root comment (email)"

            (reply_id, True) <- getLatestCommentId
            liftIO $ withEmailDaemon file $ flip errUnlessUniqueEmailNotif $
                render appRoot $ CommentDirectLinkR reply_id
        |]

    -- Not delivered by email.
    testNotification NotifWelcome =
        yit "sends the welcome message when a user is created" $ [marked|
            forM_ named_users $ \user -> do
                 user_id <- userId user
                 hasNotif user_id NotifWelcome "Thanks for registering!"
                     "welcome notification not found" False
        |]

    -- XXX: Not triggered anywhere.
    testNotification NotifBalanceLow = return ()

    -- XXX: Cannot be set by a user, so it should not be delivered by
    -- email.
    testNotification NotifUnapprovedComment =
        yit "notifies when a comment needs to be approved" $ [marked|
            let unestablished_user = L.last named_users
            loginAs unestablished_user
            postComment (enRoute NewWikiDiscussionR "about") $
                byLabel "New Topic" "unapproved comment"
            (comment_id, False) <- getLatestCommentId
            user_id <- userId unestablished_user
            hasNotif user_id NotifUnapprovedComment
                (render appRoot $ enRoute WikiCommentR "about" comment_id)
                "unapproved comment notification not found" True
        |]

    -- XXX: Not triggered anywhere.
    testNotification NotifApprovedComment = return ()

    testNotification NotifRethreadedComment = do
        yit "notifies when a comment is rethreaded" $ [marked|
            loginAs Mary
            postComment (enRoute NewWikiDiscussionR "about") $
                byLabel "New Topic" "parent comment"
            (parent_id, True) <- getLatestCommentId

            loginAs Bob
            bob_id <- userId Bob
            testDB $ updateNotifPrefs bob_id Nothing NotifRethreadedComment $
                singleton NotifDeliverWebsite
            postComment (enRoute NewWikiDiscussionR "about") $
                byLabel "New Topic" "rethreaded comment"
            (comment_id, True) <- getLatestCommentId

            loginAs AdminUser
            rethreadComment
                (render appRoot $ enRoute RethreadWikiCommentR "about" comment_id)
                (render appRoot $ enRoute WikiCommentR "about" parent_id)

            hasNotif bob_id NotifRethreadedComment
                (render appRoot $ enRoute WikiCommentR "about" comment_id)
                "rethreaded comment notification not found" True
        |]

        yit "sends an email when a comment is rethreaded" $ [marked|
            loginAs Mary
            postComment (enRoute NewWikiDiscussionR "about") $
                byLabel "New Topic" "parent comment (email)"
            (parent_id, True) <- getLatestCommentId

            loginAs Bob
            bob_id <- userId Bob
            testDB $ addAndVerifyEmail bob_id "bob@localhost"
            testDB $ updateNotifPrefs bob_id Nothing NotifRethreadedComment $
                singleton NotifDeliverEmail
            postComment (enRoute NewWikiDiscussionR "about") $
                byLabel "New Topic" "rethreaded comment (email)"
            (comment_id, True) <- getLatestCommentId

            loginAs AdminUser
            rethreadComment
                (render appRoot $ enRoute RethreadWikiCommentR "about" comment_id)
                (render appRoot $ enRoute WikiCommentR "about" parent_id)

            liftIO $ withEmailDaemon file $ flip errUnlessUniqueEmailNotif
                (render appRoot $ enRoute WikiCommentR "about" comment_id)
        |]

    -- XXX: TODO.
    testNotification NotifEditConflict = return ()

    testNotification NotifFlag = do
        yit "notifies when a comment gets flagged" $ [marked|
            loginAs Mary
            postComment (enRoute NewWikiDiscussionR "about") $
                byLabel "New Topic" "flagged comment"
            (comment_id, True) <- getLatestCommentId
            mary_id <- userId Mary
            testDB $ updateNotifPrefs mary_id Nothing NotifFlag $
                singleton NotifDeliverWebsite

            loginAs Bob
            flagComment $ render appRoot $ enRoute FlagWikiCommentR "about" comment_id

            hasNotif mary_id NotifFlag
                (render appRoot $ enRoute EditWikiCommentR "about" comment_id)
                "flagged comment notification not found" False
        |]

        yit "sends an email when a comment gets flagged" $ [marked|
            loginAs Mary
            postComment (enRoute NewWikiDiscussionR "about") $
                byLabel "New Topic" "flagged comment (email)"
            (comment_id, True) <- getLatestCommentId
            mary_id <- userId Mary
            testDB $ updateNotifPrefs mary_id Nothing NotifFlag $
                singleton NotifDeliverEmail

            loginAs Bob
            flagComment $ render appRoot $ enRoute FlagWikiCommentR "about" comment_id

            liftIO $ withEmailDaemon file $ flip errUnlessUniqueEmailNotif $
                render appRoot $ enRoute EditWikiCommentR "about" comment_id
        |]

    -- Relies on the 'NotifFlag' test.
    testNotification NotifFlagRepost = do
        yit "notifies when a flagged comment gets reposted" $ [marked|
            bob_id <- userId Bob
            testDB $ updateNotifPrefs bob_id Nothing NotifFlagRepost $
                singleton NotifDeliverWebsite

            loginAs Mary
            (comment_id, True) <- getLatestCommentId
            editComment $ render appRoot $ enRoute EditWikiCommentR "about" comment_id

            hasNotif bob_id NotifFlagRepost
                (render appRoot $ enRoute WikiCommentR "about" comment_id)
                "flagged comment reposted notification not found" False
        |]

        yit "sends an email when a flagged comment gets reposted" $ [marked|
            bob_id <- userId Bob
            testDB $ updateNotifPrefs bob_id Nothing NotifFlagRepost $
                singleton NotifDeliverEmail

            loginAs Mary
            (comment_id, True) <- getLatestCommentId
            editComment $ render appRoot $ enRoute EditWikiCommentR "about" comment_id

            liftIO $ withEmailDaemon file $ flip errUnlessUniqueEmailNotif $
                render appRoot $ enRoute WikiCommentR "about" comment_id
        |]

    testNotification NotifWikiPage = do
        yit "notifies when a wiki page is created" $ [marked|
            mary_id      <- userId Mary
            snowdrift_id <- snowdriftId
            loginAs Mary
            watch $ WatchProjectR snowdrift_id
            testDB $ updateNotifPrefs mary_id (Just snowdrift_id) NotifWikiPage $
                singleton NotifDeliverWebsite

            loginAs Bob
            newWiki snowdrift LangEn wiki_page "testing NotifWikiPage"

            hasNotif mary_id NotifWikiPage
                (render appRoot $ enRoute WikiR wiki_page)
                "new wiki page notification not found" True
        |]

        yit "sends an email when a wiki page is created" $ [marked|
            mary_id      <- userId Mary
            snowdrift_id <- snowdriftId
            loginAs Mary
            watch $ WatchProjectR snowdrift_id
            testDB $ updateNotifPrefs mary_id (Just snowdrift_id) NotifWikiPage $
                singleton NotifDeliverEmail

            loginAs Bob
            newWiki snowdrift LangEn wiki_page_email "testing NotifWikiPage (email)"

            liftIO $ withEmailDaemon file $ flip errUnlessUniqueEmailNotif $
                render appRoot $ enRoute WikiR wiki_page_email
        |]

    -- Relies on the 'NotifWikiPage' test.
    testNotification NotifWikiEdit = do
        yit "notifies when a wiki page is edited" $ [marked|
            mary_id      <- userId Mary
            snowdrift_id <- snowdriftId
            testDB $ updateNotifPrefs mary_id (Just snowdrift_id) NotifWikiEdit $
                singleton NotifDeliverWebsite

            loginAs Bob
            editWiki snowdrift LangEn wiki_page "testing NotifWikiEdit" "testing"

            hasNotif mary_id NotifWikiEdit
                (render appRoot $ enRoute WikiR wiki_page)
                "wiki page edited notification not found" True
        |]

        yit "sends an email when a wiki page is edited" $ [marked|
            mary_id      <- userId Mary
            snowdrift_id <- snowdriftId
            testDB $ updateNotifPrefs mary_id (Just snowdrift_id) NotifWikiEdit $
                singleton NotifDeliverEmail

            loginAs Bob
            editWiki snowdrift LangEn wiki_page "testing NotifWikiEdit (email)"
                "testing"

            liftIO $ withEmailDaemon file $ flip errUnlessUniqueEmailNotif $
                render appRoot $ enRoute WikiR wiki_page
        |]

    testNotification NotifBlogPost = do
        yit "notifies when a blog post is created" $ [marked|
            mary_id      <- userId Mary
            snowdrift_id <- snowdriftId
            testDB $ updateNotifPrefs mary_id (Just snowdrift_id) NotifBlogPost $
                singleton NotifDeliverWebsite

            loginAs AdminUser
            let blog_handle = "testing"
            newBlogPost blog_handle

            hasNotif mary_id NotifBlogPost
                (render appRoot $ BlogPostR snowdrift blog_handle)
                "new blog post notification not found" True
        |]

        yit "sends an email when a blog post is created" $ [marked|
            mary_id      <- userId Mary
            snowdrift_id <- snowdriftId
            testDB $ updateNotifPrefs mary_id (Just snowdrift_id) NotifBlogPost $
                singleton NotifDeliverEmail

            loginAs AdminUser
            let blog_handle = "testing-email"
            newBlogPost blog_handle

            liftIO $ withEmailDaemon file $ flip errUnlessUniqueEmailNotif $
                render appRoot $ BlogPostR snowdrift blog_handle
        |]

    testNotification NotifNewPledge = do
        yit "notifies when there is a new pledge" $ [marked|
            mary_id      <- userId Mary
            snowdrift_id <- snowdriftId
            testDB $ updateNotifPrefs mary_id (Just snowdrift_id) NotifNewPledge $
                singleton NotifDeliverWebsite

            loginAs Bob
            let tshares = shpack shares
            pledge tshares

            bob_id <- userId Bob
            hasNotif mary_id NotifNewPledge
                ("user" <> (shpack $ keyToInt64 bob_id) <>
                 " pledged [" <> tshares <> " shares]")
                "new pledge notification not found" True
        |]

        yit "sends an email when there is a new pledge" $ [marked|
            mary_id      <- userId Mary
            snowdrift_id <- snowdriftId
            testDB $ updateNotifPrefs mary_id (Just snowdrift_id) NotifNewPledge $
                singleton NotifDeliverEmail

            loginAs Bob
            pledge $ shpack (0 :: Int)  -- drop it first
            let tshares = shpack shares_email
            pledge tshares

            bob_id <- userId Bob
            liftIO $ withEmailDaemon file $ flip errUnlessUniqueEmailNotif $
                "user" <> (shpack $ keyToInt64 bob_id) <>
                " pledged [" <> tshares <> " shares]"
        |]

    testNotification NotifUpdatedPledge = do
        yit "notifies when the pledge is updated" $ [marked|
            mary_id      <- userId Mary
            snowdrift_id <- snowdriftId
            testDB $ updateNotifPrefs mary_id (Just snowdrift_id) NotifUpdatedPledge $
                singleton NotifDeliverWebsite

            loginAs Bob
            let tshares = shpack shares'
            pledge tshares

            bob_id <- userId Bob
            hasNotif mary_id NotifUpdatedPledge
                ("user" <> (shpack $ keyToInt64 bob_id) <>
                 " added " <> (shpack $ shares' - shares) <>
                 " share, changing the total to [" <> tshares <> " shares]")
                "pledge updated notification not found" True
        |]

        yit "sends an email when the pledge is updated" $ [marked|
            mary_id      <- userId Mary
            snowdrift_id <- snowdriftId
            testDB $ updateNotifPrefs mary_id (Just snowdrift_id) NotifUpdatedPledge $
                singleton NotifDeliverEmail

            loginAs Bob
            let tshares = shpack shares_email'
            pledge tshares

            bob_id <- userId Bob
            liftIO $ withEmailDaemon file $ flip errUnlessUniqueEmailNotif $
                "user" <> (shpack $ keyToInt64 bob_id) <>
                " added " <> (shpack $ shares' - shares) <>
                " share, changing the total to [" <> tshares <> " shares]"
        |]

    testNotification NotifDeletedPledge = do
        yit "notifies when a user stops supporting the project" $ [marked|
            mary_id      <- userId Mary
            snowdrift_id <- snowdriftId
            testDB $ updateNotifPrefs mary_id (Just snowdrift_id) NotifDeletedPledge $
                singleton NotifDeliverWebsite

            loginAs Bob
            pledge $ shpack (0 :: Int)

            bob_id <- userId Bob
            hasNotif mary_id NotifDeletedPledge
                ("user" <> (shpack $ keyToInt64 bob_id) <>
                 " is no longer supporting the [project]")
                "pledge deleted notification not found" True
        |]

        yit "sends an email when a user stops supporting the project" $ [marked|
            mary_id      <- userId Mary
            snowdrift_id <- snowdriftId
            testDB $ updateNotifPrefs mary_id (Just snowdrift_id) NotifDeletedPledge $
                singleton NotifDeliverEmail

            loginAs Bob
            pledge $ shpack shares  -- pledge again before dropping
            pledge $ shpack (0 :: Int)

            bob_id <- userId Bob
            liftIO $ withEmailDaemon file $ flip errUnlessUniqueEmailNotif $
                "user" <> (shpack $ keyToInt64 bob_id) <>
                " is no longer supporting the [project]"
        |]
