{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE GADTs             #-}

module NotifyTest (notifySpecs) where

import           Import                               (notDistinctFrom)
import           TestImport                           hiding ((=.), update, notificationContent, delete)
import           Model.Language
import           Model.Notification

import           Control.Applicative                  ((<$>))
import           Control.Concurrent                   (threadDelay)
import           Control.Monad                        (unless)
import           Database.Esqueleto                   hiding (get, isNothing)
import           Database.Esqueleto.Internal.Language (From)
import           Data.Foldable                        (forM_)
import qualified Data.List                            as L
import           Data.List.NonEmpty                   (NonEmpty)
import qualified Data.List.NonEmpty                   as NonEmpty
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

-- Copied from 'Model.User' but without the constraint in the result.
deleteNotifPrefs :: UserId -> Maybe ProjectId -> NotificationType -> SqlPersistM ()
deleteNotifPrefs user_id mproject_id notif_type =
    delete $ from $ \unp ->
    where_ $ unp ^. UserNotificationPrefUser ==. val user_id
         &&. unp ^. UserNotificationPrefProject `notDistinctFrom` val mproject_id
         &&. unp ^. UserNotificationPrefType ==. val notif_type

-- Copied from 'Model.User' but without the constraint in the result.
updateNotifPrefs :: UserId -> Maybe ProjectId -> NotificationType
                 -> NonEmpty NotificationDelivery -> SqlPersistM ()
updateNotifPrefs user_id mproject_id notif_type notif_delivs = do
    deleteNotifPrefs user_id mproject_id notif_type
    forM_ notif_delivs $
        insert_ . UserNotificationPref user_id mproject_id notif_type

singleton :: a -> NonEmpty a
singleton = flip (NonEmpty.:|) []

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

watch :: Route App -> YesodExample App ()
watch route = [marked|
     withStatus 302 False $ request $ do
         setMethod "POST"
         setUrl route
|]

newBlogPost :: Text -> YesodExample App ()
newBlogPost page = [marked|
    let route = NewBlogPostR snowdrift
    get200 route

    withStatus 302 False $ request $ do
        addNonce
        setMethod "POST"
        setUrl route
        byLabel "Title for this blog post" "testing"
        byLabel "Handle for the URL" page
        byLabel "Content" "testing"
        addPostParam "mode" "post"
|]

pledge :: Text -> YesodExample App ()
pledge shares = [marked|
    get200 $ ProjectR snowdrift

    let route = UpdateSharesR snowdrift
    withStatus 200 False $ request $ do
        setUrl route
        setMethod "GET"
        addGetParam "_hasdata" ""
        addGetParam "f1" shares

    withStatus 302 False $ request $ do
        addNonce
        setMethod "POST"
        setUrl route
        addPostParam "f1" shares
        addPostParam "confirm" "yes!"
|]

named_users :: [NamedUser]
named_users = [minBound .. maxBound]

notifySpecs :: AppConfig DefaultEnv a -> Spec
notifySpecs AppConfig {..} = do
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
    enRoute con    = con snowdrift LangEn
    absolute route = appRoot <> "/" <> route
    render         = absolute . T.intercalate "/" . fst . renderRoute

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
            postComment (enRoute NewWikiDiscussionR "about") $
                byLabel "New Topic" "unapproved comment"
            (comment_id, False) <- getLatestCommentId
            user_id <- userId unestablished_user
            hasNotif user_id NotifUnapprovedComment
                (render $ enRoute WikiCommentR "about" comment_id)
                "unapproved comment notification not found" True
        |]

    -- XXX: Not triggered anywhere.
    testNotification NotifApprovedComment = return ()

    testNotification NotifRethreadedComment =
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
                (render $ enRoute RethreadWikiCommentR "about" comment_id)
                (render $ enRoute WikiCommentR "about" parent_id)

            hasNotif bob_id NotifRethreadedComment
                (render $ enRoute WikiCommentR "about" comment_id)
                "rethreaded comment notification not found" True
        |]

    -- XXX: TODO.
    testNotification NotifEditConflict = return ()

    testNotification NotifFlag =
        yit "notifies when a comment gets flagged" $ [marked|
            loginAs Mary
            postComment (enRoute NewWikiDiscussionR "about") $
                byLabel "New Topic" "flagged comment"
            (comment_id, True) <- getLatestCommentId
            mary_id <- userId Mary
            testDB $ updateNotifPrefs mary_id Nothing NotifFlag $
                singleton NotifDeliverWebsite

            loginAs Bob
            flagComment $ render $ enRoute FlagWikiCommentR "about" comment_id

            hasNotif mary_id NotifFlag
                (render $ enRoute EditWikiCommentR "about" comment_id)
                "flagged comment notification not found" False
        |]

    -- Relies on the 'NotifFlag' test.
    testNotification NotifFlagRepost =
        yit "notifies when a flagged comment gets reposted" $ [marked|
            bob_id <- userId Bob
            testDB $ updateNotifPrefs bob_id Nothing NotifFlagRepost $
                singleton NotifDeliverWebsite

            loginAs Mary
            (comment_id, True) <- getLatestCommentId
            editComment $ render $ enRoute EditWikiCommentR "about" comment_id

            hasNotif bob_id NotifFlagRepost
                (render $ enRoute WikiCommentR "about" comment_id)
                "flagged comment reposted notification not found" False
        |]

    testNotification NotifWikiPage =
        yit "notifies when a wiki page is created" $ [marked|
            mary_id      <- userId Mary
            snowdrift_id <- snowdriftId
            loginAs Mary
            watch $ WatchProjectR snowdrift_id
            testDB $ updateNotifPrefs mary_id (Just snowdrift_id) NotifWikiPage $
                singleton NotifDeliverWebsite

            loginAs Bob
            newWiki snowdrift LangEn wiki_page "testing NotifWikiPage"

            hasNotif mary_id NotifWikiPage (render $ enRoute WikiR wiki_page)
                "new wiki page notification not found" True
        |]

    -- Relies on the 'NotifWikiPage' test.
    testNotification NotifWikiEdit =
        yit "notifies when a wiki page is edited" $ [marked|
            mary_id      <- userId Mary
            snowdrift_id <- snowdriftId
            testDB $ updateNotifPrefs mary_id (Just snowdrift_id) NotifWikiEdit $
                singleton NotifDeliverWebsite

            loginAs Bob
            editWiki snowdrift LangEn wiki_page "testing NotifWikiEdit" "testing"

            hasNotif mary_id NotifWikiEdit (render $ enRoute WikiR wiki_page)
                "wiki page edited notification not found" True
        |]

    testNotification NotifBlogPost =
        yit "notifies when a blog post is created" $ [marked|
            mary_id      <- userId Mary
            snowdrift_id <- snowdriftId
            testDB $ updateNotifPrefs mary_id (Just snowdrift_id) NotifBlogPost $
                singleton NotifDeliverWebsite

            loginAs AdminUser
            let blog_handle = "testing"
            newBlogPost blog_handle

            hasNotif mary_id NotifBlogPost
                (render $ BlogPostR snowdrift blog_handle)
                "new blog post notification not found" True
        |]

    testNotification NotifNewPledge =
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

    testNotification NotifUpdatedPledge =
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

    testNotification NotifDeletedPledge =
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
