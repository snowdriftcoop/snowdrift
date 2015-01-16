{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE GADTs             #-}

module NotifyTest (notifySpecs) where

import           Import                               (Established(..))
import           TestImport                           hiding ((=.), update, (</>), Update)
import           Model.Language
import           Model.Notification

import           Control.Exception                    (bracket)
import           Control.Monad                        (void, unless)
import           Database.Esqueleto
import           Database.Esqueleto.Internal.Language (Update, From)
import           Data.Foldable                        (forM_)
import qualified Data.List                            as L
import           Data.Monoid                          ((<>))
import           Data.Text                            (Text)
import qualified Data.Text                            as T
import qualified Data.Text.IO                         as T
import           System.FilePath                      ((</>))
import           System.Process                       (spawnProcess, terminateProcess)
import           Yesod.Default.Config                 (AppConfig (..), DefaultEnv (..))
import           Yesod.Markdown                       (unMarkdown, Markdown)

updateUser :: UserId -> [SqlExpr (Update User)] -> SqlPersistM ()
updateUser user_id xs =
    update $ \ u -> do
        set u xs
        where_ $ u ^. UserId ==. val user_id

unestablish :: UserId -> SqlPersistM ()
unestablish user_id = do
    delete $ from $ \ me ->
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

countWebsiteNotif :: Bool -> UserId -> NotificationType -> Text
                  -> SqlPersistM Int
countWebsiteNotif with_delay user_id notif_type text =
    (if with_delay then withDelay else id) $ do
        contents <- fmap (T.unwords . map (unMarkdown . unValue)) $
                    select $ notificationContent user_id notif_type
        return $ T.count text contents

notificationContent :: From query expr backend (expr (Entity Notification))
                    => Key User -> NotificationType
                    -> query (expr (Value Markdown))
notificationContent user_id notif_type =
    from $ \n -> do
        where_ $ n ^. NotificationTo   ==. val user_id
             &&. n ^. NotificationType ==. val notif_type
        return $ n ^. NotificationContent

countEmailNotif :: FilePath -> Text -> IO Int
countEmailNotif file text = do
    contents <- T.readFile file
    return $ T.count text contents

errUnlessUnique :: Monad m => Int -> String -> String -> m ()
errUnlessUnique c text loc =
    unless (c == 1) $
        error $ "'" <> text <> "' appears " <> show c <> " times "
             <> "in " <> loc

errUnlessUniqueWebsiteNotif :: Bool -> UserId -> NotificationType -> Text
                            -> SqlPersistM ()
errUnlessUniqueWebsiteNotif with_delay user_id notif_type text = do
    c <- countWebsiteNotif with_delay user_id notif_type text
    errUnlessUnique c (T.unpack text) "the notification table"

errUnlessUniqueEmailNotif :: FilePath -> Text -> IO ()
errUnlessUniqueEmailNotif file text = do
    c <- countEmailNotif file text
    errUnlessUnique c (T.unpack text) file

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

    errUnlessUniqueWebsiteNotif' with_delay user_id notif_type text =
        testDB $ errUnlessUniqueWebsiteNotif with_delay user_id notif_type text
    errUnlessUniqueEmailNotif' =
        liftIO . withEmailDaemon file . flip errUnlessUniqueEmailNotif

    testNotification NotifEligEstablish = do
        yit "notifies on establishment" $ [marked|
            forM_ (L.init named_users) $ \user -> do
                user_id <- userId user
                loginAs AdminUser
                establish user_id

                errUnlessUniqueWebsiteNotif' False user_id NotifEligEstablish $
                    render appRoot $ HonorPledgeR
                loginAs user
                acceptHonorPledge
        |]

        yit "send an email when a user is eligible for establishment" $ [marked|
            mary_id <- userId Mary
            testDB $ unestablish mary_id
            testDB $ addAndVerifyEmail mary_id "mary@localhost"
            loginAs AdminUser
            establish mary_id
            errUnlessUniqueEmailNotif'
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
            errUnlessUniqueWebsiteNotif' True mary_id NotifReply $
                render appRoot $ CommentDirectLinkR reply_id
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
            errUnlessUniqueEmailNotif' $
                render appRoot $ CommentDirectLinkR reply_id
        |]

    -- Not delivered by email.
    testNotification NotifWelcome =
        yit "sends the welcome message when a user is created" $ [marked|
            forM_ named_users $ \user -> do
                 user_id <- userId user
                 errUnlessUniqueWebsiteNotif' False user_id NotifWelcome $
                     "Thanks for registering!"
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
            errUnlessUniqueWebsiteNotif' True user_id NotifUnapprovedComment $
                render appRoot $ enRoute WikiCommentR "about" comment_id
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

            errUnlessUniqueWebsiteNotif' True bob_id NotifRethreadedComment $
                render appRoot $ enRoute WikiCommentR "about" comment_id
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

            errUnlessUniqueEmailNotif' $
                render appRoot $ enRoute WikiCommentR "about" comment_id
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

            errUnlessUniqueWebsiteNotif' False mary_id NotifFlag $
                render appRoot $ enRoute EditWikiCommentR "about" comment_id
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

            errUnlessUniqueEmailNotif' $
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

            errUnlessUniqueWebsiteNotif' False bob_id NotifFlagRepost $
                render appRoot $ enRoute WikiCommentR "about" comment_id
        |]

        yit "sends an email when a flagged comment gets reposted" $ [marked|
            bob_id <- userId Bob
            testDB $ updateNotifPrefs bob_id Nothing NotifFlagRepost $
                singleton NotifDeliverEmail

            loginAs Mary
            (comment_id, True) <- getLatestCommentId
            editComment $ render appRoot $ enRoute EditWikiCommentR "about" comment_id

            errUnlessUniqueEmailNotif' $
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

            errUnlessUniqueWebsiteNotif' True mary_id NotifWikiPage $
                render appRoot $ enRoute WikiR wiki_page
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

            errUnlessUniqueEmailNotif' $
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

            errUnlessUniqueWebsiteNotif' True mary_id NotifWikiEdit $
                render appRoot $ enRoute WikiR wiki_page
        |]

        yit "sends an email when a wiki page is edited" $ [marked|
            mary_id      <- userId Mary
            snowdrift_id <- snowdriftId
            testDB $ updateNotifPrefs mary_id (Just snowdrift_id) NotifWikiEdit $
                singleton NotifDeliverEmail

            loginAs Bob
            editWiki snowdrift LangEn wiki_page "testing NotifWikiEdit (email)"
                "testing"

            errUnlessUniqueEmailNotif' $
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

            errUnlessUniqueWebsiteNotif' True mary_id NotifBlogPost $
                render appRoot $ BlogPostR snowdrift blog_handle
        |]

        yit "sends an email when a blog post is created" $ [marked|
            mary_id      <- userId Mary
            snowdrift_id <- snowdriftId
            testDB $ updateNotifPrefs mary_id (Just snowdrift_id) NotifBlogPost $
                singleton NotifDeliverEmail

            loginAs AdminUser
            let blog_handle = "testing-email"
            newBlogPost blog_handle

            errUnlessUniqueEmailNotif' $
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
            errUnlessUniqueWebsiteNotif' True mary_id NotifNewPledge $
                "user" <> (shpack $ keyToInt64 bob_id) <>
                " pledged [" <> tshares <> " shares]"
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
            errUnlessUniqueEmailNotif' $
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
            errUnlessUniqueWebsiteNotif' True mary_id NotifUpdatedPledge $
                "user" <> (shpack $ keyToInt64 bob_id) <>
                " added " <> (shpack $ shares' - shares) <>
                " share, changing the total to [" <> tshares <> " shares]"
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
            errUnlessUniqueEmailNotif' $
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
            errUnlessUniqueWebsiteNotif' True mary_id NotifDeletedPledge $
                "user" <> (shpack $ keyToInt64 bob_id) <>
                " is no longer supporting the [project]"
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
            errUnlessUniqueEmailNotif' $
                "user" <> (shpack $ keyToInt64 bob_id) <>
                " is no longer supporting the [project]"
        |]
