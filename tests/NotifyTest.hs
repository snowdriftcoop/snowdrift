{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE GADTs             #-}

module NotifyTest (notifySpecs) where

import Prelude
import Import (Established(..), Role (..), key)
import TestImport hiding ((=.), update, Update)

import Control.Monad (unless)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans.Resource (ResourceT)
import Data.Foldable (forM_)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Database.Esqueleto hiding (exists)
import Database.Esqueleto.Internal.Language (From)
import Yesod.Default.Config (AppConfig (..), DefaultEnv (..))
import Yesod.Markdown (unMarkdown, Markdown (..))
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Database.Persist as P

import Model.Currency (Milray (..))
import Model.Language
import Model.Notification

import PPrint

unestablish :: UserId -> SqlPersistM ()
unestablish user_id = do
    P.deleteWhere [ManualEstablishmentEstablishedUser P.==. user_id]
    P.update user_id [UserEstablished P.=. EstUnestablished]

addAndVerifyEmail :: UserId -> Text -> SqlPersistM ()
addAndVerifyEmail user_id email =
    P.update user_id [ UserEmail P.=. (Just email)
                     , UserEmail_verified P.=. True ]

data DelayStatus = WithDelay | WithoutDelay

countWebsiteNotif :: (UserId -> a -> SqlQuery (SqlExpr (Value Markdown)))
                  -> DelayStatus -> UserId -> a -> Text
                  -> SqlPersistM Int
countWebsiteNotif f delay_status user_id notif_type text =
    (case delay_status of
         WithDelay    -> withDelay
         WithoutDelay -> id) $ do
        contents <- fmap (T.unwords . map (unMarkdown . unValue)) $
                    select $ f user_id notif_type
        return $ T.count text contents

countUserWebsiteNotif :: DelayStatus -> UserId -> UserNotificationType -> Text
                      -> SqlPersistM Int
countUserWebsiteNotif = countWebsiteNotif userNotificationContent

countProjectWebsiteNotif :: DelayStatus -> UserId -> ProjectNotificationType -> Text
                         -> SqlPersistM Int
countProjectWebsiteNotif = countWebsiteNotif projectNotificationContent

notificationContent :: ( From query expr backend (expr (Entity val))
                       , PersistEntity val, PersistField a
                       , PersistField b, PersistField c )
                    => EntityField val a -> EntityField val b
                    -> EntityField val c -> a -> b
                    -> query (expr (Value c))
notificationContent notif_to_con notif_type_con notif_content_con
                    notif_to_val notif_type_val =
    from $ \n -> do
        where_ $ n ^. notif_to_con   ==. val notif_to_val
             &&. n ^. notif_type_con ==. val notif_type_val
        return $ n ^. notif_content_con

userNotificationContent
    :: From query expr backend (expr (Entity UserNotification))
    => UserId -> UserNotificationType
    -> query (expr (Value Markdown))
userNotificationContent =
    notificationContent
        UserNotificationTo UserNotificationType UserNotificationContent

projectNotificationContent
    :: From query expr backend (expr (Entity ProjectNotification))
    => UserId -> ProjectNotificationType
    -> query (expr (Value Markdown))
projectNotificationContent =
    notificationContent
        ProjectNotificationTo ProjectNotificationType ProjectNotificationContent

countEmailNotif :: FileName -> Text -> Example Int
countEmailNotif file text = liftIO $ do
    contents <- T.readFile $ T.unpack $ unFileName file
    return $ T.count text contents

errUnless :: Monad m => Int -> Int -> String -> String -> m ()
errUnless expected actual text loc =
    unless (expected == actual) $
        error $ "'" <> text <> "' appears " <> show actual <> " times "
             <> "in " <> loc <> "; expected " <> show expected

errUnlessUnique, errWhenExists :: Monad m => Int -> String -> String -> m ()
errUnlessUnique = errUnless 1
errWhenExists   = errUnless 0

newtype TableName = TableName { unTableName :: Text }

user_notification, project_notification :: TableName
user_notification    = TableName "user_notification"
project_notification = TableName "project_notification"

errWebsiteNotif :: (DelayStatus -> UserId -> a -> Text -> SqlPersistM Int)
                -> (Int -> String -> String -> SqlPersistM ())
                -> TableName -> DelayStatus -> UserId -> a -> Text
                -> SqlPersistM ()
errWebsiteNotif count_f err_f table delay_status user_id notif_type text = do
    c <- count_f delay_status user_id notif_type text
    err_f c (T.unpack text) $ "the " <> show (unTableName table) <> " table"

errUnlessUniqueUserWebsiteNotif, errWhenExistsUserWebsiteNotif
    :: DelayStatus -> UserId -> UserNotificationType -> Text -> SqlPersistM ()
errUnlessUniqueUserWebsiteNotif =
    errWebsiteNotif
        countUserWebsiteNotif errUnlessUnique user_notification
errWhenExistsUserWebsiteNotif =
    errWebsiteNotif
        countUserWebsiteNotif errWhenExists user_notification

errUnlessUniqueProjectWebsiteNotif, errWhenExistsProjectWebsiteNotif
    :: DelayStatus -> UserId -> ProjectNotificationType -> Text -> SqlPersistM ()
errUnlessUniqueProjectWebsiteNotif =
    errWebsiteNotif
        countProjectWebsiteNotif errUnlessUnique project_notification
errWhenExistsProjectWebsiteNotif =
    errWebsiteNotif
        countProjectWebsiteNotif errWhenExists project_notification

errEmailNotif :: (Int -> String -> String -> Example ())
              -> FileName -> Text -> Example ()
errEmailNotif f file text = do
    c <- countEmailNotif file text
    f c (T.unpack text) $ T.unpack $ unFileName file

errUnlessUniqueEmailNotif, errWhenExistsEmailNotif
    :: FileName -> Text -> Example ()
errUnlessUniqueEmailNotif = errEmailNotif errUnlessUnique
errWhenExistsEmailNotif   = errEmailNotif errWhenExists

insertRole, deleteRole
    :: ProjectId -> UserId -> Role -> SqlPersistM ()
insertRole project_id user_id role =
    insert_ $ ProjectUserRole project_id user_id role

deleteRole project_id user_id role =
    delete $ from $ \p -> do
        where_ $ p ^. ProjectUserRoleProject ==. val project_id
             &&. p ^. ProjectUserRoleUser    ==. val user_id
             &&. p ^. ProjectUserRoleRole    ==. val role

errWebsiteNotif'
    :: (DelayStatus -> UserId -> a -> Text -> SqlPersistM ())
    -> DelayStatus -> UserId -> a -> Text -> Example ()
errWebsiteNotif' function delay_status user_id notif_type text =
    testDB $ function delay_status user_id notif_type text

errWhenExistsUserWebsiteNotif', errUnlessUniqueUserWebsiteNotif'
    :: DelayStatus -> UserId -> UserNotificationType -> Text -> Example ()
errWhenExistsUserWebsiteNotif'   = errWebsiteNotif' errWhenExistsUserWebsiteNotif
errUnlessUniqueUserWebsiteNotif' = errWebsiteNotif' errUnlessUniqueUserWebsiteNotif

errWhenExistsProjectWebsiteNotif', errUnlessUniqueProjectWebsiteNotif'
    :: DelayStatus -> UserId -> ProjectNotificationType -> Text -> Example ()
errWhenExistsProjectWebsiteNotif'   = errWebsiteNotif' errWhenExistsProjectWebsiteNotif
errUnlessUniqueProjectWebsiteNotif' = errWebsiteNotif' errUnlessUniqueProjectWebsiteNotif

errEmailNotif'
    :: String -> FileName -> (FileName -> Text -> Example ())
    -> Text -> Example ()
errEmailNotif' str file function = withEmailDaemon str file . flip function

errWhenExistsEmailNotif', errUnlessUniqueEmailNotif'
    :: String -> FileName -> Text -> Example ()
errWhenExistsEmailNotif'   str file = errEmailNotif' str file errWhenExistsEmailNotif
errUnlessUniqueEmailNotif' str file = errEmailNotif' str file errUnlessUniqueEmailNotif

watchProject :: Login user => user -> ProjectId -> Example () -> Example ()
watchProject user project_id action = do
    loginAs user
    changeWatchStatus $ WatchProjectR project_id
    action
    loginAs user
    changeWatchStatus $ UnwatchProjectR project_id

errorUnlessExistDefaultProjectNotifPrefs :: UserId -> ProjectId -> SqlPersistM ()
errorUnlessExistDefaultProjectNotifPrefs user_id project_id =
    forM_ [ (NotifWikiEdit,      ProjectNotifDeliverWebsite)
          , (NotifWikiPage,      ProjectNotifDeliverWebsite)
          , (NotifBlogPost,      ProjectNotifDeliverWebsiteAndEmail)
          , (NotifNewPledge,     ProjectNotifDeliverWebsite)
          , (NotifUpdatedPledge, ProjectNotifDeliverWebsite)
          , (NotifDeletedPledge, ProjectNotifDeliverWebsite) ] $
        \(notif_type, notif_deliv) -> do
            exists <- fmap (>0) $
                P.count [ProjectNotificationPrefUser P.==. user_id
                        ,ProjectNotificationPrefProject P.==. project_id
                        ,ProjectNotificationPrefType P.==. notif_type
                        ,ProjectNotificationPrefDelivery P.==. notif_deliv]
            unless exists $
                error $ "project notification preference with"
                     <> " user "      <> pprint user_id
                     <> ", project "  <> pprint project_id
                     <> ", type "     <> pprint notif_type
                     <> ", delivery " <> pprint notif_deliv
                     <> " does not exist"

-- | Update user notification preferences and delete the present ones,
-- so they don't affect the tests.
resetUserNotifPrefs :: UserId -> UserNotificationType
                    -> UserNotificationDelivery
                    -> ReaderT SqlBackend (NoLoggingT (ResourceT IO)) ()
resetUserNotifPrefs user_id notif_type notif_deliv = do
    delete $ from $ \unp ->
        where_ $ unp ^. UserNotificationPrefUser ==. val user_id
    updateUserNotifPrefs user_id notif_type notif_deliv

-- | Update project notification preferences and delete the present ones,
-- so they don't affect the tests.
resetProjectNotifPrefs :: UserId -> ProjectId -> ProjectNotificationType
                       -> ProjectNotificationDelivery
                       -> ReaderT SqlBackend (NoLoggingT (ResourceT IO)) ()
resetProjectNotifPrefs user_id project_id notif_type notif_deliv = do
    delete $ from $ \pnp ->
        where_ $ pnp ^. ProjectNotificationPrefUser ==. val user_id
    updateProjectNotifPrefs user_id project_id notif_type notif_deliv

notifySpecs :: AppConfig DefaultEnv a -> FileName -> Spec
notifySpecs AppConfig {..} file = do
    -- Note that since we rely on 'Bounded' here, the order of the
    -- 'NotificationType' value constructors is important for some of
    -- these tests.  For example, 'NotifWikiEdit' must not be tested
    -- prior to 'NotifWikiPage'.  Otherwise, there will be nothing to
    -- edit.
    ydescribe "user notifications" $
        mapM_ testUserNotification [minBound .. maxBound]
    ydescribe "project notifications" $
        mapM_ testProjectNotification [minBound .. maxBound]

  where
    wiki_page      = "testing"

    wiki_page_email = "testing-email"

    wiki_page_self       = "testing-self"
    wiki_page_self_email = "testing-self-email"

    editComment' route = editComment route "testing"

    testUserNotification NotifEligEstablish = do
        yit "notifies on establishment" $ do
            forM_ (L.init named_users) $ \user -> do
                user_id <- userId user
                loginAs AdminUser
                establish user_id

                errUnlessUniqueUserWebsiteNotif'
                    WithoutDelay user_id NotifEligEstablish $
                        render appRoot $ HonorPledgeR
                loginAs user
                acceptHonorPledge

        yit "send an email when a user is eligible for establishment" $ do
            mary_id <- userId Mary
            testDB $ unestablish mary_id
            testDB $ addAndVerifyEmail mary_id "mary@localhost"
            loginAs AdminUser
            establish mary_id
            errUnlessUniqueEmailNotif'
                "sent the user notification to mary@localhost"
                file
                "You are now eligible to become an *established* user"
            loginAs Mary
            acceptHonorPledge

    testUserNotification NotifReply = do
        yit "notifies on reply" $ do
            loginAs Mary
            postComment (enRoute NewWikiDiscussionR "about") $
                byLabel "New Topic" "root comment"

            mary_id <- userId Mary
            testDB $ resetUserNotifPrefs mary_id NotifReply
                UserNotifDeliverWebsite

            loginAs Bob
            (comment_id, True) <- getLatestCommentId
            postComment
                (enRoute ReplyWikiCommentR "about" comment_id) $
                    byLabel "Reply" "reply to the root comment"

            (reply_id, True) <- getLatestCommentId
            errUnlessUniqueUserWebsiteNotif' WithDelay mary_id NotifReply $
                render appRoot $ CommentDirectLinkR reply_id

        yit "doesn't notify when replying to yourself" $ do
            loginAs Mary
            postComment (enRoute NewWikiDiscussionR "about") $
                byLabel "New Topic" "root comment (self)"

            mary_id <- userId Mary
            testDB $ resetUserNotifPrefs mary_id NotifReply
                UserNotifDeliverWebsite

            (comment_id, True) <- getLatestCommentId
            postComment
                (enRoute ReplyWikiCommentR "about" comment_id) $
                    byLabel "Reply" "reply to the root comment (self)"

            (reply_id, True) <- getLatestCommentId
            errWhenExistsUserWebsiteNotif' WithDelay mary_id NotifReply $
                render appRoot $ CommentDirectLinkR reply_id

        yit "sends an email on reply" $ do
            loginAs Mary
            postComment (enRoute NewWikiDiscussionR "about") $
                byLabel "New Topic" "root comment (email)"

            mary_id <- userId Mary
            testDB $ resetUserNotifPrefs mary_id NotifReply
                UserNotifDeliverEmail

            loginAs Bob
            (comment_id, True) <- getLatestCommentId
            postComment
                (enRoute ReplyWikiCommentR "about" comment_id) $
                    byLabel "Reply" "reply to the root comment (email)"

            (reply_id, True) <- getLatestCommentId
            errUnlessUniqueEmailNotif'
                "sent the user notification to mary@localhost"
                file $
                render appRoot $ CommentDirectLinkR reply_id

        yit "doesn't send an email when replying to yourself" $ do
            loginAs Mary
            postComment (enRoute NewWikiDiscussionR "about") $
                byLabel "New Topic" "root comment (email, self)"

            mary_id <- userId Mary
            testDB $ resetUserNotifPrefs mary_id NotifReply
                UserNotifDeliverEmail

            (comment_id, True) <- getLatestCommentId
            postComment
                (enRoute ReplyWikiCommentR "about" comment_id) $
                    byLabel "Reply" "reply to the root comment (email, self)"

            (reply_id, True) <- getLatestCommentId
            errWhenExistsEmailNotif'
                "iteration finished" file $
                render appRoot $ CommentDirectLinkR reply_id

    -- Not delivered by email.
    testUserNotification NotifWelcome =
        yit "sends the welcome message when a user is created" $ do
            forM_ named_users $ \user -> do
                 user_id <- userId user
                 errUnlessUniqueUserWebsiteNotif' WithoutDelay user_id NotifWelcome
                     "Thanks for registering!"

    -- XXX: Not triggered anywhere.
    testUserNotification NotifBalanceLow = return ()

    -- XXX: Cannot be set by a user, so it should not be delivered by
    -- email.
    testUserNotification NotifUnapprovedComment =
        yit "notifies when a comment needs to be approved" $ do
            let unestablished_user = L.last named_users
            loginAs unestablished_user
            postComment (enRoute NewWikiDiscussionR "about") $
                byLabel "New Topic" "unapproved comment"
            (comment_id, False) <- getLatestCommentId
            user_id <- userId unestablished_user
            errUnlessUniqueUserWebsiteNotif' WithDelay user_id NotifUnapprovedComment $
                render appRoot $ enRoute WikiCommentR "about" comment_id

    -- XXX: Not triggered anywhere.
    testUserNotification NotifApprovedComment = return ()

    testUserNotification NotifRethreadedComment = do
        yit "notifies when a comment is rethreaded" $ do
            loginAs Mary
            postComment (enRoute NewWikiDiscussionR "about") $
                byLabel "New Topic" "parent comment"
            (parent_id, True) <- getLatestCommentId

            loginAs Bob
            bob_id <- userId Bob
            testDB $ resetUserNotifPrefs bob_id NotifRethreadedComment
                UserNotifDeliverWebsite
            postComment (enRoute NewWikiDiscussionR "about") $
                byLabel "New Topic" "rethreaded comment"
            (comment_id, True) <- getLatestCommentId

            loginAs AdminUser
            rethreadComment
                (render appRoot $ enRoute RethreadWikiCommentR "about" comment_id)
                (render appRoot $ enRoute WikiCommentR "about" parent_id)

            errUnlessUniqueUserWebsiteNotif' WithDelay bob_id NotifRethreadedComment $
                render appRoot $ enRoute WikiCommentR "about" comment_id

        yit "doesn't notify when rethreading your own comment" $ do
            loginAs Mary
            mary_id      <- userId Mary
            snowdrift_id <- snowdriftId
            testDB $ insertRole snowdrift_id mary_id Moderator

            postComment (enRoute NewWikiDiscussionR "about") $
                byLabel "New Topic" "parent comment (self)"
            (parent_id, True) <- getLatestCommentId

            testDB $ resetUserNotifPrefs mary_id NotifRethreadedComment
                UserNotifDeliverWebsite
            postComment (enRoute NewWikiDiscussionR "about") $
                byLabel "New Topic" "rethreaded comment (self)"
            (comment_id, True) <- getLatestCommentId

            rethreadComment
                (render appRoot $ enRoute RethreadWikiCommentR "about" comment_id)
                (render appRoot $ enRoute WikiCommentR "about" parent_id)

            errWhenExistsUserWebsiteNotif'
                WithDelay mary_id NotifRethreadedComment $
                    render appRoot $ enRoute WikiCommentR "about" comment_id

            testDB $ deleteRole snowdrift_id mary_id Moderator

        yit "sends an email when a comment is rethreaded" $ do
            loginAs Mary
            postComment (enRoute NewWikiDiscussionR "about") $
                byLabel "New Topic" "parent comment (email)"
            (parent_id, True) <- getLatestCommentId

            loginAs Bob
            bob_id <- userId Bob
            testDB $ addAndVerifyEmail bob_id "bob@localhost"
            testDB $ resetUserNotifPrefs bob_id NotifRethreadedComment
                UserNotifDeliverEmail
            postComment (enRoute NewWikiDiscussionR "about") $
                byLabel "New Topic" "rethreaded comment (email)"
            (comment_id, True) <- getLatestCommentId

            loginAs AdminUser
            rethreadComment
                (render appRoot $ enRoute RethreadWikiCommentR "about" comment_id)
                (render appRoot $ enRoute WikiCommentR "about" parent_id)

            errUnlessUniqueEmailNotif'
                "sent the user notification to bob@localhost"
                file $
                render appRoot $ enRoute WikiCommentR "about" comment_id

        yit "doesn't send an email when rethreading your own comment" $ do
            loginAs Mary
            mary_id      <- userId Mary
            snowdrift_id <- snowdriftId
            testDB $ insertRole snowdrift_id mary_id Moderator

            postComment (enRoute NewWikiDiscussionR "about") $
                byLabel "New Topic" "parent comment (email, self)"
            (parent_id, True) <- getLatestCommentId

            testDB $ resetUserNotifPrefs mary_id NotifRethreadedComment
                UserNotifDeliverEmail
            postComment (enRoute NewWikiDiscussionR "about") $
                byLabel "New Topic" "rethreaded comment (email, self)"
            (comment_id, True) <- getLatestCommentId

            rethreadComment
                (render appRoot $ enRoute RethreadWikiCommentR "about" comment_id)
                (render appRoot $ enRoute WikiCommentR "about" parent_id)

            errWhenExistsEmailNotif'
                "iteration finished" file $
                render appRoot $ enRoute WikiCommentR "about" comment_id

            testDB $ deleteRole snowdrift_id mary_id Moderator

    -- XXX: TODO.
    testUserNotification NotifEditConflict = return ()

    testUserNotification NotifFlag = do
        yit "notifies when a comment gets flagged" $ do
            loginAs Mary
            postComment (enRoute NewWikiDiscussionR "about") $
                byLabel "New Topic" "flagged comment"
            (comment_id, True) <- getLatestCommentId
            mary_id <- userId Mary
            testDB $ resetUserNotifPrefs mary_id NotifFlag
                UserNotifDeliverWebsite

            loginAs Bob
            flagComment $ render appRoot $ enRoute FlagWikiCommentR "about" comment_id

            errUnlessUniqueUserWebsiteNotif' WithoutDelay mary_id NotifFlag $
                render appRoot $ enRoute EditWikiCommentR "about" comment_id

        yit "sends an email when a comment gets flagged" $ do
            loginAs Mary
            postComment (enRoute NewWikiDiscussionR "about") $
                byLabel "New Topic" "flagged comment (email)"
            (comment_id, True) <- getLatestCommentId
            mary_id <- userId Mary
            testDB $ resetUserNotifPrefs mary_id NotifFlag
                UserNotifDeliverEmail

            loginAs Bob
            flagComment $ render appRoot $ enRoute FlagWikiCommentR "about" comment_id

            errUnlessUniqueEmailNotif'
                "sent the user notification to mary@localhost"
                file $
                render appRoot $ enRoute EditWikiCommentR "about" comment_id

    -- Relies on the 'NotifFlag' test.
    testUserNotification NotifFlagRepost = do
        yit "notifies when a flagged comment gets reposted" $ do
            bob_id <- userId Bob
            testDB $ resetUserNotifPrefs bob_id NotifFlagRepost
                UserNotifDeliverWebsite

            loginAs Mary
            (comment_id, True) <- getLatestCommentId
            editComment' $ render appRoot $ enRoute EditWikiCommentR "about" comment_id

            errUnlessUniqueUserWebsiteNotif' WithoutDelay bob_id NotifFlagRepost $
                render appRoot $ enRoute WikiCommentR "about" comment_id

        yit "sends an email when a flagged comment gets reposted" $ do
            bob_id <- userId Bob
            loginAs Bob
            testDB $ resetUserNotifPrefs bob_id NotifFlagRepost
                UserNotifDeliverEmail
            (comment_id, True) <- getLatestCommentId
            flagComment $ render appRoot $ enRoute FlagWikiCommentR "about" comment_id

            loginAs Mary
            editComment' $ render appRoot $ enRoute EditWikiCommentR "about" comment_id

            errUnlessUniqueEmailNotif'
                "sent the user notification to bob@localhost"
                file $
                render appRoot $ enRoute WikiCommentR "about" comment_id

    testProjectNotification NotifWikiPage = do
        yit "notifies when a wiki page is created" $ do
            mary_id      <- userId Mary
            snowdrift_id <- snowdriftId
            watchProject Mary snowdrift_id $ do
                testDB $ resetProjectNotifPrefs mary_id snowdrift_id NotifWikiPage
                    ProjectNotifDeliverWebsite

                loginAs Bob
                newWiki snowdrift LangEn wiki_page "testing NotifWikiPage"

                errUnlessUniqueProjectWebsiteNotif' WithDelay mary_id NotifWikiPage $
                    render appRoot $ enRoute WikiR wiki_page

        yit "doesn't notify when a wiki page is created by you" $ do
            mary_id      <- userId Mary
            snowdrift_id <- snowdriftId
            watchProject Mary snowdrift_id $ do
                testDB $ resetProjectNotifPrefs mary_id snowdrift_id NotifWikiPage
                    ProjectNotifDeliverWebsite

                newWiki snowdrift LangEn wiki_page_self "testing NotifWikiPage (self)"

                errWhenExistsProjectWebsiteNotif'
                    WithDelay mary_id NotifWikiPage $
                        render appRoot $ enRoute WikiR wiki_page_self

        yit "sends an email when a wiki page is created" $ do
            mary_id      <- userId Mary
            snowdrift_id <- snowdriftId
            watchProject Mary snowdrift_id $ do
                testDB $ resetProjectNotifPrefs mary_id snowdrift_id NotifWikiPage
                    ProjectNotifDeliverEmail

                loginAs Bob
                newWiki snowdrift LangEn wiki_page_email "testing NotifWikiPage (email)"

                errUnlessUniqueEmailNotif'
                    "sent the project notification to mary@localhost"
                    file $
                    render appRoot $ enRoute WikiR wiki_page_email

        yit "doesn't send an email when a wiki page is created by you" $ do
            mary_id      <- userId Mary
            snowdrift_id <- snowdriftId
            watchProject Mary snowdrift_id $ do
                testDB $ resetProjectNotifPrefs mary_id snowdrift_id NotifWikiPage
                    ProjectNotifDeliverEmail

                newWiki snowdrift LangEn wiki_page_self_email "testing NotifWikiPage (email, self)"

                errWhenExistsEmailNotif'
                    "iteration finished"
                    file $
                    render appRoot $ enRoute WikiR wiki_page_self_email

    -- Relies on the 'NotifWikiPage' test.
    testProjectNotification NotifWikiEdit = do
        yit "notifies when a wiki page is edited" $ do
            mary_id      <- userId Mary
            snowdrift_id <- snowdriftId
            watchProject Mary snowdrift_id $ do
                testDB $ resetProjectNotifPrefs mary_id snowdrift_id NotifWikiEdit
                    ProjectNotifDeliverWebsite

                loginAs Bob
                editWiki snowdrift LangEn wiki_page "testing NotifWikiEdit" "testing"

                errUnlessUniqueProjectWebsiteNotif' WithDelay mary_id NotifWikiEdit $
                    render appRoot $ enRoute WikiR wiki_page

        yit "doesn't notify when a wiki page is edited by you" $ do
            mary_id      <- userId Mary
            snowdrift_id <- snowdriftId
            watchProject Mary snowdrift_id $ do
                testDB $ insertRole snowdrift_id mary_id Moderator
                testDB $ resetProjectNotifPrefs mary_id snowdrift_id NotifWikiEdit
                    ProjectNotifDeliverWebsite

                loginAs Mary
                editWiki snowdrift LangEn wiki_page_self "testing NotifWikiEdit" "testing (self)"

                errWhenExistsProjectWebsiteNotif' WithDelay mary_id NotifWikiEdit $
                    render appRoot $ enRoute WikiR wiki_page_self

                testDB $ deleteRole snowdrift_id mary_id Moderator

        yit "sends an email when a wiki page is edited" $ do
            mary_id      <- userId Mary
            snowdrift_id <- snowdriftId
            watchProject Mary snowdrift_id $ do
                testDB $ resetProjectNotifPrefs mary_id snowdrift_id NotifWikiEdit
                    ProjectNotifDeliverEmail

                loginAs Bob
                editWiki snowdrift LangEn wiki_page "testing NotifWikiEdit (email)"
                    "testing"

                errUnlessUniqueEmailNotif'
                    "sent the project notification to mary@localhost"
                    file $
                    render appRoot $ enRoute WikiR wiki_page

        yit "doesn't send an email when a wiki page is edited by you" $ do
            mary_id      <- userId Mary
            snowdrift_id <- snowdriftId
            watchProject Mary snowdrift_id $ do
                testDB $ insertRole snowdrift_id mary_id Moderator
                testDB $ resetProjectNotifPrefs mary_id snowdrift_id NotifWikiEdit
                    ProjectNotifDeliverEmail

                loginAs Mary
                editWiki snowdrift LangEn wiki_page_self "testing NotifWikiEdit (email, self)"
                    "testing"

                errWhenExistsEmailNotif'
                    "iteration finished" file $
                    render appRoot $ enRoute WikiR wiki_page_self

                testDB $ deleteRole snowdrift_id mary_id Moderator

    testProjectNotification NotifBlogPost = do
        yit "notifies when a blog post is created" $ do
            mary_id      <- userId Mary
            snowdrift_id <- snowdriftId
            watchProject Mary snowdrift_id $ do
                testDB $ resetProjectNotifPrefs mary_id snowdrift_id NotifBlogPost
                    ProjectNotifDeliverWebsite

                loginAs AdminUser
                let blog_handle = "testing"
                newBlogPost blog_handle

                errUnlessUniqueProjectWebsiteNotif' WithDelay mary_id NotifBlogPost $
                    render appRoot $ BlogPostR snowdrift blog_handle

        yit "doesn't notify when a blog post is created by you" $ do
            mary_id      <- userId Mary
            snowdrift_id <- snowdriftId
            watchProject Mary snowdrift_id $ do
                testDB $ insertRole snowdrift_id mary_id TeamMember
                testDB $ resetProjectNotifPrefs mary_id snowdrift_id NotifBlogPost
                    ProjectNotifDeliverWebsite

                loginAs Mary
                let blog_handle = "testing-self"
                newBlogPost blog_handle

                errWhenExistsProjectWebsiteNotif' WithDelay mary_id NotifBlogPost $
                    render appRoot $ BlogPostR snowdrift blog_handle

                testDB $ deleteRole snowdrift_id mary_id TeamMember

        yit "sends an email when a blog post is created" $ do
            mary_id      <- userId Mary
            snowdrift_id <- snowdriftId
            watchProject Mary snowdrift_id $ do
                testDB $ resetProjectNotifPrefs mary_id snowdrift_id NotifBlogPost
                    ProjectNotifDeliverEmail

                loginAs AdminUser
                let blog_handle = "testing-email"
                newBlogPost blog_handle

                errUnlessUniqueEmailNotif'
                    "sent the project notification to mary@localhost"
                    file $
                    render appRoot $ BlogPostR snowdrift blog_handle

        yit "doesn't send an email when a blog post is created by you" $ do
            mary_id      <- userId Mary
            snowdrift_id <- snowdriftId
            watchProject Mary snowdrift_id $ do
                testDB $ insertRole snowdrift_id mary_id TeamMember
                testDB $ resetProjectNotifPrefs mary_id snowdrift_id NotifBlogPost
                    ProjectNotifDeliverEmail

                loginAs Mary
                let blog_handle = "testing-self-email"
                newBlogPost blog_handle

                errWhenExistsEmailNotif'
                    "iteration finished" file $
                    render appRoot $ BlogPostR snowdrift blog_handle

                testDB $ deleteRole snowdrift_id mary_id TeamMember

    testProjectNotification NotifNewPledge = do
        yit "notifies when there is a new pledge" (return ())
        yit "doesn't notify when you make a new pledge" (return ())
        yit "sends an email when there is a new pledge" (return ())
        yit "doesn't send an email when you make a new pledge" (return ())

    testProjectNotification NotifUpdatedPledge = do
        yit "notifies when the pledge is updated" (return ())
        yit "doesn't notify when the pledge is updated by you" (return ())
        yit "sends an email when the pledge is updated" (return ())
        yit "doesn't send an email when the pledge is updated by you" (return ())

    testProjectNotification NotifDeletedPledge = do
        yit "notifies when a user stops supporting the project" (return ())
        yit "doesn't notify when you stop supporting the project" (return ())
        yit "sends an email when a user stops supporting the project" (return ())
        yit "doesn't send an email when you stop supporting the project" (return ())
        yit ("project notification preferences are checked per-project " <>
             "before inserting the defaults on 'watch'") $ do
            mary_id <- userId Mary
            -- Start watching the Snowdrift.coop project without doing
            -- anything.
            snowdrift_id <- snowdriftId
            watchProject Mary snowdrift_id $ return ()
            -- Check that some (not necessarily default) notification
            -- preferences are already there.
            testDB $ do
                exists <- fmap (>0) $
                    P.count [ProjectNotificationPrefUser P.==. mary_id
                            ,ProjectNotificationPrefProject P.==. snowdrift_id]
                unless exists $
                    error $ "cannot find project notification preferences for"
                         <> " user "     <> pprint mary_id
                         <> ", project " <> pprint snowdrift_id

            -- Start watching another project and check that the
            -- default notification preferences are there.
            test_project_id <- testDB $ do
                now <- liftIO $ getCurrentTime
                insert $ Project now "test" "test" "test" (Markdown "test")
                                 (key $ PersistInt64 1) (Milray 42) Nothing
                                 Nothing (key $ PersistInt64 1) True Nothing
            watchProject Mary test_project_id $ return ()
            testDB $ errorUnlessExistDefaultProjectNotifPrefs mary_id test_project_id
    testProjectNotification NotifVolunteerApp = return ()
