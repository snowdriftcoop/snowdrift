{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module EmailTest (emailSpecs) where

import Import (Established(..))
import TestImport hiding ((=.), update, Update, (</>))
import Model.Language
import Model.Notification (NotificationType(..), NotificationDelivery(..))

import Control.Applicative ((<$>))
import Control.Exception (bracket)
import Control.Monad (void, unless)
import Database.Esqueleto
import Database.Esqueleto.Internal.Language (Update)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.FilePath ((</>))
import System.Process (spawnProcess, terminateProcess)
import Yesod.Default.Config (AppConfig (..), DefaultEnv (..))

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

hasEmailNotif :: FilePath -> Text -> IO Bool
hasEmailNotif file text = (text `Text.isInfixOf`) <$> (Text.readFile file)

errUnlessEmailNotif :: FilePath -> Text -> IO ()
errUnlessEmailNotif file text = do
    has_notif <- hasEmailNotif file text
    unless has_notif $
        error $ "could not find " <> Text.unpack text <> " in " <> file

emailSpecs :: AppConfig DefaultEnv a -> FilePath -> Spec
emailSpecs AppConfig {..} file = do
    ydescribe "email notifications" $ do
        mapM_ testEmail [minBound .. maxBound]

  where
    wiki_page = "testing-email"

    -- Not delivered by email.
    testEmail NotifWelcome           = return ()

    testEmail NotifEligEstablish =
        yit "send an email when a user is eligible for establishment" $ [marked|
            mary_id <- userId Mary
            testDB $ unestablish mary_id
            testDB $ addAndVerifyEmail mary_id "mary@localhost"
            loginAs AdminUser
            establish mary_id
            liftIO $ withEmailDaemon file $ flip errUnlessEmailNotif
                "You are now eligible to become an *established* user"
            loginAs Mary
            acceptHonorPledge
        |]

    -- XXX: Not triggered anywhere.
    testEmail NotifBalanceLow        = return ()

    -- XXX: Cannot be set by a user, so it should not be delivered by
    -- email.
    testEmail NotifUnapprovedComment = return ()

    -- XXX: Not triggered anywhere.
    testEmail NotifApprovedComment   = return ()

    testEmail NotifRethreadedComment =
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

            liftIO $ withEmailDaemon file $ flip errUnlessEmailNotif
                (render appRoot $ enRoute WikiCommentR "about" comment_id)
        |]

    testEmail NotifReply =
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
            liftIO $ withEmailDaemon file $ flip errUnlessEmailNotif $
                render appRoot $ CommentDirectLinkR reply_id
        |]

    -- XXX: TODO.
    testEmail NotifEditConflict      = return ()

    testEmail NotifFlag =
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

            liftIO $ withEmailDaemon file $ flip errUnlessEmailNotif $
                render appRoot $ enRoute EditWikiCommentR "about" comment_id
        |]

    -- Relies on the 'NotifFlag' test.
    testEmail NotifFlagRepost =
        yit "sends an email when a flagged comment gets reposted" $ [marked|
            bob_id <- userId Bob
            testDB $ updateNotifPrefs bob_id Nothing NotifFlagRepost $
                singleton NotifDeliverEmail

            loginAs Mary
            (comment_id, True) <- getLatestCommentId
            editComment $ render appRoot $ enRoute EditWikiCommentR "about" comment_id

            liftIO $ withEmailDaemon file $ flip errUnlessEmailNotif $
                render appRoot $ enRoute WikiCommentR "about" comment_id
        |]

    testEmail NotifWikiPage =
        yit "sends an email when a wiki page is created" $ [marked|
            mary_id      <- userId Mary
            snowdrift_id <- snowdriftId
            loginAs Mary
            watch $ WatchProjectR snowdrift_id
            testDB $ updateNotifPrefs mary_id (Just snowdrift_id) NotifWikiPage $
                singleton NotifDeliverEmail

            loginAs Bob
            newWiki snowdrift LangEn wiki_page "testing NotifWikiPage (email)"

            liftIO $ withEmailDaemon file $ flip errUnlessEmailNotif $
                render appRoot $ enRoute WikiR wiki_page
        |]

    testEmail NotifWikiEdit          = return ()
    testEmail NotifBlogPost          = return ()
    testEmail NotifNewPledge         = return ()
    testEmail NotifUpdatedPledge     = return ()
    testEmail NotifDeletedPledge     = return ()
