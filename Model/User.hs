module Model.User
    ( UserMap
    , UserUpdate (..)
    , ChangePassword (..)
    , SetPassword (..)
    , NotificationSender (..)
    , NotificationReceiver (..)
    -- Utility functions
    , anonymousUser
    , deletedUser
    , curUserIsEligibleEstablish
    , deleteArchivedNotificationsDB
    , deleteNotificationDB
    , deleteNotificationsDB
    , projectNotificationPref
    , updateUserPreview
    , userCanAddTag
    , userCanCloseComment
    , userCanEditComment
    , userCanEditWikiPage
    , userIsEligibleEstablish
    , userIsEstablished
    , userIsModerator
    , userIsUnestablished
    , userDisplayName
    , userNotificationPref
    -- Database actions
    , archiveNotificationsDB
    , deleteFromEmailVerification
    , deleteCommentsDB
    , deleteBlogPostsDB
    , deleteWikiEditsDB
    , deleteUserDB
    , eligEstablishUserDB
    , establishUserDB
    , fetchAllUserRolesDB
    , fetchCurUserRolesDB
    , fetchNumUnreadNotificationsDB
    , fetchNumUnviewedCommentsOnProjectWikiPagesDB
    , fetchNumUnviewedWikiEditsOnProjectDB
    , fetchUserArchivedNotificationsDB
    , fetchUserEmail
    , fetchUserEmailVerified
    , fetchUserNotificationsDB
    , fetchUserNotificationPrefDB
    , fetchUserPledgesDB
    , fetchUserProjectsAndRolesDB
    , fetchUserRolesDB
    , fetchUsersInDB
    , fetchUsersByNotifPrefDB
    , fetchUserWatchingProjectsDB
    , fetchVerEmail
    , fromEmailVerification
    , sendPreferredNotificationDB
    , unarchiveNotificationsDB
    , updateUserDB
    , updateUserPasswordDB
    , updateNotificationPrefDB
    , userCanDeleteCommentDB
    , userHasRoleDB
    , userHasRolesAnyDB
    , userIsAffiliatedWithProjectDB
    , userIsProjectAdminDB
    , userIsProjectModeratorDB
    , userIsProjectTeamMemberDB
    , userIsWatchingProjectDB
    , userMaybeViewProjectCommentsDB
    , userReadNotificationsDB
    , userReadVolunteerApplicationsDB
    , userUnwatchProjectDB
    , userViewCommentsDB
    , userViewWikiEditsDB
    , userWatchProjectDB
    , verifyEmailDB
    -- Unsorted
    , canCurUserMakeEligible
    , canMakeEligible
    ) where

import Import


import Model.Comment
import Model.Comment.Sql
import Model.Notification
import Model.User.Internal hiding (UserNotificationPref)
import Model.User.Sql
import Model.Wiki.Sql

import           Database.Esqueleto.Internal.Language (From)
import qualified Data.Foldable      as F
import qualified Data.List          as L
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.Map           as M
import qualified Data.Set           as S
import qualified Data.Text          as T
import           Control.Monad.Trans.Reader (ReaderT)

-- anonymousUser is a special user for items posted by visitors who are not
-- logged in, such as posting to /contact for a project
anonymousUser :: UserId
anonymousUser = key $ PersistInt64 (-1)

-- When a user deletes the account, all their comments are assigned to
-- this user.
deletedUser :: UserId
deletedUser = key $ PersistInt64 (-2)

type UserMap = Map UserId User

--------------------------------------------------------------------------------
-- Utility functions

userCanAddTag :: User -> Bool
userCanAddTag = userIsEstablished

-- TODO: what should this be?
-- Aaron says: I think we should allow established to mark as closed,
-- but only *affiliated* OR the original poster should do so in one step,
-- otherwise, the marking of closed should require *moderator* confirmation…
-- We should also have a re-open function.
-- There are now comments discussing these things on the site.
userCanCloseComment :: User -> Bool
userCanCloseComment = userIsEstablished

-- | Can this User edit this Comment?
userCanEditComment :: UserId -> Comment -> Bool
userCanEditComment user_id = (user_id ==) . commentUser

userCanEditWikiPage :: User -> Bool
userCanEditWikiPage = userIsEstablished

-- | Is the user established?
userIsEstablished :: User -> Bool
userIsEstablished = estIsEstablished . userEstablished

-- | Is the user eligible for establishment?
userIsEligibleEstablish :: User -> Bool
userIsEligibleEstablish = estIsEligible . userEstablished

userIsModerator :: UserId -> DB Bool
userIsModerator user_id = elem Moderator <$> fetchAllUserRolesDB user_id

-- | Is the user unestablished?
userIsUnestablished :: User -> Bool
userIsUnestablished = estIsUnestablished . userEstablished

-- | Is the current user eligible for establishment?
curUserIsEligibleEstablish :: Handler Bool
curUserIsEligibleEstablish = maybe False (userIsEligibleEstablish . entityVal) <$> maybeAuth

-- | Get a User's public display name (defaults to userN if no name has been set).
userDisplayName :: Entity User -> Text
userDisplayName (Entity user_id user) = fromMaybe ("user" <> toPathPiece user_id) (userName user)

-- | Apply a UserUpdate in memory, for preview. For this reason,
-- userUpdateNotificationPreferences doesn't need to be touched.
updateUserPreview :: UserUpdate -> User -> User
updateUserPreview UserUpdate{..} user = user
    { userName               = userUpdateName
    , userAvatar             = userUpdateAvatar
    , userEmail              = userUpdateEmail
    , userIrcNick            = userUpdateIrcNick
    , userStatement          = userUpdateStatement
    , userBlurb              = userUpdateBlurb
    }

--------------------------------------------------------------------------------
-- Database functions

fetchUsersInDB :: [UserId] -> DB [Entity User]
fetchUsersInDB user_ids = selectList [UserId <-. user_ids] []

fetchUserWatchingProjectsDB :: UserId -> DB [Entity Project]
fetchUserWatchingProjectsDB user_id =
    select $ from $ \ (uwp,p) -> do
        where_ $ uwp ^. UserWatchingProjectUser    ==. val user_id
             &&. uwp ^. UserWatchingProjectProject ==. p ^. ProjectId

        return p

updateUserDB :: UserId -> UserUpdate -> DB ()
updateUserDB user_id UserUpdate{..} = do
    update $ \u -> do
     set u $ [ UserName               =. val userUpdateName
             , UserAvatar             =. val userUpdateAvatar
             , UserEmail              =. val userUpdateEmail
             , UserIrcNick            =. val userUpdateIrcNick
             , UserStatement          =. val userUpdateStatement
             , UserBlurb              =. val userUpdateBlurb
             ]
     where_ (u ^. UserId ==. val user_id)

updateUserPasswordDB :: UserId -> Maybe Text -> Maybe Text -> DB ()
updateUserPasswordDB user_id hash salt =
    update $ \u -> do
        set u $ [ UserHash =. val hash
                , UserSalt =. val salt ]
        where_ $ u ^. UserId ==. val user_id

fromEmailVerification :: From query expr backend (expr (Entity EmailVerification))
                      => UserId -> query ()
fromEmailVerification user_id =
    from $ \ev -> where_ $ ev ^. EmailVerificationUser ==. val user_id

deleteFromEmailVerification :: MonadIO m => UserId -> SqlPersistT m ()
deleteFromEmailVerification user_id =
    delete $ fromEmailVerification user_id

replaceWithDeletedUser
    :: (PersistEntity val, PersistEntityBackend val ~ SqlBackend)
    => EntityField val UserId -> UserId -> DB ()
replaceWithDeletedUser con user_id =
    update $ \c -> do
        set c $ [con =. val deletedUser]
        where_ $ c ^. con ==. val user_id

deleteCommentsDB :: UserId -> DB ()
deleteCommentsDB = replaceWithDeletedUser CommentUser

deleteBlogPostsDB :: UserId -> DB ()
deleteBlogPostsDB = replaceWithDeletedUser BlogPostUser

deleteWikiEditsDB :: UserId -> DB ()
deleteWikiEditsDB = replaceWithDeletedUser WikiEditUser

deleteUserDB :: UserId -> DB ()
deleteUserDB user_id = do
    deleteCommentsDB user_id
    deleteBlogPostsDB user_id
    deleteWikiEditsDB user_id
    deleteCascade user_id

fetchVerEmail :: Text -> UserId -> DB (Maybe Text)
fetchVerEmail ver_uri user_id = do
    emails <- fmap (map unValue) $
              select $ from $ \ev -> do
                  where_ $ ev ^. EmailVerificationUser ==. val user_id
                       &&. ev ^. EmailVerificationUri  ==. val ver_uri
                  return $ ev ^. EmailVerificationEmail
    if L.null emails
        then return $ Nothing
        else return $ Just $ L.head emails

verifyEmailDB :: MonadIO m => UserId -> ReaderT SqlBackend m ()
verifyEmailDB user_id = do
    update $ \u -> do
        set u $ [UserEmail_verified =. val True]
        where_ $ u ^. UserId ==. val user_id
    deleteFromEmailVerification user_id

-- | Establish a user, given their eligible-timestamp and reason for
-- eligibility. Mark all unapproved comments of theirs as approved.
establishUserDB :: UserId -> UTCTime -> Text -> DB ()
establishUserDB user_id elig_time reason = do
    est_time <- liftIO getCurrentTime

    let est = EstEstablished elig_time est_time reason
    update $ \u -> do
        set u [ UserEstablished =. val est ]
        where_ (u ^. UserId ==. val user_id)

    approveUnapprovedComments est_time
  where
    approveUnapprovedComments :: UTCTime -> DB ()
    approveUnapprovedComments est_time =
        update $ \c -> do
            set c [ CommentApprovedTs =. just (val est_time)
                  , CommentApprovedBy =. just (val user_id)
                  ]
            where_ $
                c ^. CommentUser ==. val user_id &&.
                exprCommentUnapproved c

-- | Make a user eligible for establishment. Put a notification in their inbox
-- instructing them to read and accept the honor pledge.
eligEstablishUserDB :: Text -> UserId -> UserId -> Text -> SDB ()
eligEstablishUserDB honor_pledge establisher_id user_id reason = do
    elig_time <- liftIO getCurrentTime
    let est = EstEligible elig_time reason
    lift $
        update $ \u -> do
        set u [UserEstablished =. val est]
        where_ (u ^. UserId ==. val user_id)

    lift $ insert_ $ ManualEstablishment user_id establisher_id
    sendPreferredNotificationDB
        (Just $ NotificationSender establisher_id)
        (NotificationReceiver user_id)
        NotifEligEstablish
        Nothing
        Nothing
        content
  where
    content :: Markdown
    content = Markdown $ T.unlines
        [ "You are now eligible to become an *established* user."
        , ""
        , "After you [accept the honor pledge](" <> honor_pledge <>
          "), you can comment and take other actions on the site without moderation."
        ]

-- | Get a User's Roles in a Project.
fetchUserRolesDB :: UserId -> ProjectId -> DB [Role]
fetchUserRolesDB user_id project_id = fmap (map unValue) $
    select $
        from $ \r -> do
        where_ (r ^. ProjectUserRoleProject ==. val project_id &&.
                r ^. ProjectUserRoleUser ==. val user_id)
        return $ r ^. ProjectUserRoleRole

-- | Get all of a User's Roles, across all Projects.
fetchAllUserRolesDB :: UserId -> DB [Role]
fetchAllUserRolesDB user_id = fmap unwrapValues $
    selectDistinct $
        from $ \pur -> do
        where_ (pur ^. ProjectUserRoleUser ==. val user_id)
        return (pur ^. ProjectUserRoleRole)

-- | Get the current User's Roles in a Project.
fetchCurUserRolesDB :: ProjectId -> Handler [Role]
fetchCurUserRolesDB project_id = maybeAuthId >>= \case
    Nothing -> return []
    Just user_id -> runDB $ fetchUserRolesDB user_id project_id

-- | Does this User have this Role in this Project?
userHasRoleDB :: Role -> UserId -> ProjectId -> DB Bool
userHasRoleDB role user_id = fmap (elem role) . fetchUserRolesDB user_id

-- | Does this User have any of these Roles in this Project?
userHasRolesAnyDB :: [Role] -> UserId -> ProjectId -> DB Bool
userHasRolesAnyDB roles user_id project_id = (or . flip map roles . flip elem) <$> fetchUserRolesDB user_id project_id

-- | Like the name says.
-- TODO: Why does it run map (second return) on the result? That's just
-- creating a bunch of 1-element lists.
fetchUserPledgesDB :: UserId -> DB [(Entity Project, [Entity Pledge])]
fetchUserPledgesDB user_id =
    fmap (map (second return)) $ do
    select $ from $ \(project `InnerJoin` pledge) -> do
        on_ $ project ^. ProjectId ==. pledge ^. PledgeProject
        where_ $ pledge ^. PledgeUser ==. val user_id
        return (project, pledge)

-- | Get all Projects this User is affiliated with, along with each Role.
fetchUserProjectsAndRolesDB :: UserId -> DB (Map (Entity Project) (Set Role))
fetchUserProjectsAndRolesDB user_id = fmap buildMap $
    select $
        from $ \(p `InnerJoin` pur) -> do
        on_ (p ^. ProjectId ==.  pur ^. ProjectUserRoleProject)
        where_ (pur ^. ProjectUserRoleUser ==. val user_id)
        return (p, pur ^. ProjectUserRoleRole)
  where
    buildMap :: [(Entity Project, Value Role)] -> Map (Entity Project) (Set Role)
    buildMap = foldr (\(p, Value r) -> M.insertWith (<>) p (S.singleton r)) mempty

userIsProjectAdminDB :: UserId -> ProjectId -> DB Bool
userIsProjectAdminDB = userHasRoleDB Admin

userIsProjectTeamMemberDB :: UserId -> ProjectId -> DB Bool
userIsProjectTeamMemberDB = userHasRoleDB TeamMember

userIsProjectModeratorDB :: UserId -> ProjectId -> DB Bool
userIsProjectModeratorDB = userHasRoleDB Moderator

-- | A User is affiliated with a Project if they have *any* Role.
userIsAffiliatedWithProjectDB :: UserId -> ProjectId -> DB Bool
userIsAffiliatedWithProjectDB = userHasRolesAnyDB [minBound..maxBound]

-- | Check if the current User can make the given User eligible for establishment.
-- This is True if the current User is a Moderator of any Project, and the given User
-- is Unestablished.
canCurUserMakeEligible :: UserId -> Handler Bool
canCurUserMakeEligible user_id = maybeAuthId >>= maybe (return False) (canMakeEligible user_id)

-- | Check if a User (FIRST ARG) can be made eligible by another User (SECOND ARG).
canMakeEligible :: UserId -> UserId -> Handler Bool
canMakeEligible establishee_id establisher_id = do
    (establishee, establisher_is_mod) <- runYDB $ (,)
        <$> get404 establishee_id
        <*> (userIsModerator establisher_id)
    return $ userIsUnestablished establishee && establisher_is_mod

-- | Fetch a User's unarchived private Notifications.
fetchUserNotificationsDB :: UserId -> DB [Entity Notification]
fetchUserNotificationsDB = fetchNotifs (not_ . (^. NotificationArchived))

-- | Fetch a User's archived private Notifications.
fetchUserArchivedNotificationsDB :: UserId -> DB [Entity Notification]
fetchUserArchivedNotificationsDB = fetchNotifs (^. NotificationArchived)

-- | Abstract fetching archived/unarchived Notifications. Unexported.
fetchNotifs :: (SqlExpr (Entity Notification) -> SqlExpr (Value Bool)) -> UserId -> DB [Entity Notification]
fetchNotifs cond user_id =
    select $
    from $ \n -> do
    where_ $
        n ^. NotificationTo ==. val user_id &&.
        cond n
    orderBy [desc (n ^. NotificationCreatedTs)]
    return n

deleteEventNotificationSentDB :: NotificationId -> DB ()
deleteEventNotificationSentDB notif_id =
    delete $ from $ \ens ->
        where_ $ ens ^. EventNotificationSentNotification ==. val notif_id

deleteUnapprovedCommentNotificationDB :: NotificationId -> DB ()
deleteUnapprovedCommentNotificationDB notif_id =
    delete $ from $ \ucn ->
        where_ $ ucn ^. UnapprovedCommentNotificationNotification ==.
                 val notif_id

deleteNotificationDB :: NotificationId -> DB ()
deleteNotificationDB notif_id = do
    deleteEventNotificationSentDB notif_id
    deleteUnapprovedCommentNotificationDB notif_id
    delete $ from $ \n ->
        where_ $ n ^. NotificationId ==. val notif_id

deleteNotificationsDB :: UserId -> DB ()
deleteNotificationsDB user_id = do
    notifs <- fetchUserNotificationsDB user_id
    forM_ notifs $ \(Entity notif_id _) ->
        deleteNotificationDB notif_id

deleteArchivedNotificationsDB :: UserId -> DB ()
deleteArchivedNotificationsDB user_id = do
    notifs <- fetchUserArchivedNotificationsDB user_id
    forM_ notifs $ \(Entity notif_id _) ->
        deleteNotificationDB notif_id

archiveNotificationsDB :: UserId -> DB ()
archiveNotificationsDB user_id = do
    notifs <- fetchUserNotificationsDB user_id
    forM_ notifs $ \(Entity notif_id _) ->
        archiveNotificationDB notif_id

unarchiveNotificationsDB :: UserId -> DB ()
unarchiveNotificationsDB user_id = do
    notifs <- fetchUserArchivedNotificationsDB user_id
    forM_ notifs $ \(Entity notif_id _) ->
        unarchiveNotificationDB notif_id

deleteNotifPrefs :: UserId -> Maybe ProjectId -> NotificationType -> DB ()
deleteNotifPrefs user_id mproject_id notif_type =
    delete $ from $ \ unp ->
        where_ $ unp ^. UserNotificationPrefUser ==. val user_id
             &&. unp ^. UserNotificationPrefProject `notDistinctFrom` val mproject_id
             &&. unp ^. UserNotificationPrefType ==. val notif_type

updateNotifPrefs :: UserId -> Maybe ProjectId -> NotificationType
                 -> NonEmpty NotificationDelivery -> DB ()
updateNotifPrefs user_id mproject_id notif_type notif_delivs = do
    deleteNotifPrefs user_id mproject_id notif_type
    F.forM_ notif_delivs $
        insert_ . UserNotificationPref user_id mproject_id notif_type

updateNotificationPrefDB :: UserId -> Maybe ProjectId -> NotificationType
                         -> Maybe (NonEmpty NotificationDelivery) -> DB ()
updateNotificationPrefDB user_id mproject_id notif_type notif_delivs =
    maybe (deleteNotifPrefs user_id mproject_id notif_type)
          (updateNotifPrefs user_id mproject_id notif_type)
          notif_delivs

userWatchProjectDB :: UserId -> ProjectId -> DB ()
userWatchProjectDB user_id project_id =
    void $ insertUnique $ UserWatchingProject user_id project_id

userUnwatchProjectDB :: UserId -> ProjectId -> DB ()
userUnwatchProjectDB user_id project_id =
    deleteBy $ UniqueUserWatchingProject user_id project_id

userIsWatchingProjectDB :: UserId -> ProjectId -> DB Bool
userIsWatchingProjectDB user_id project_id = maybe (False) (const True) <$> getBy (UniqueUserWatchingProject user_id project_id)

-- | Mark all given Comments as viewed by the given User.
userViewCommentsDB :: UserId -> [CommentId] -> DB ()
userViewCommentsDB user_id unfiltered_comment_ids = filteredCommentIds >>= userViewCommentsDB'
  where
    filteredCommentIds = fmap (map unValue) $
        select $
        from $ \vc -> do
        where_ $
            vc ^. ViewCommentUser ==. val user_id &&.
            vc ^. ViewCommentComment `notIn` valList unfiltered_comment_ids
        return (vc ^. ViewCommentComment)

    userViewCommentsDB' :: [CommentId] -> DB ()
    userViewCommentsDB' comment_ids = void (insertMany (map (ViewComment user_id) comment_ids))

-- | Mark all given Comments as viewed by the given User, if they are watching
-- the given Project.
userMaybeViewProjectCommentsDB :: UserId -> ProjectId -> [CommentId] -> DB ()
userMaybeViewProjectCommentsDB user_id project_id comment_ids = do
    ok <- userIsWatchingProjectDB user_id project_id
    when ok $
        userViewCommentsDB user_id comment_ids

-- | Mark all WikiEdits made on the given WikiPage as viewed by the given User.
userViewWikiEditsDB :: UserId -> WikiPageId -> DB ()
userViewWikiEditsDB user_id wiki_page_id = unviewedWikiEdits >>= viewWikiEdits
  where
    unviewedWikiEdits :: DB [WikiEditId]
    unviewedWikiEdits = fmap (map unValue) $
        select $
        from $ \we -> do
        where_ $
            we ^. WikiEditPage ==. val wiki_page_id &&.
            we ^. WikiEditId `notIn` exprUserViewedWikiEdits user_id
        return (we ^. WikiEditId)

    viewWikiEdits :: [WikiEditId] -> DB ()
    viewWikiEdits = mapM_ (insert_ . ViewWikiEdit user_id)

-- | Update this User's read notifications timestamp.
userReadNotificationsDB :: UserId -> DB ()
userReadNotificationsDB user_id = liftIO getCurrentTime >>= \now -> do
    update $ \u -> do
    set u [UserReadNotifications =. val now]
    where_ (u ^. UserId ==. val user_id)

-- | Update this User's read applications timestamp.
userReadVolunteerApplicationsDB :: UserId -> DB ()
userReadVolunteerApplicationsDB user_id = liftIO getCurrentTime >>= \now -> do
    update $ \u -> do
    set u [UserReadApplications =. val now]
    where_ (u ^. UserId ==. val user_id)

-- | Is this User allowed to delete this Comment?
-- If it has any replies at all - no.
userCanDeleteCommentDB :: UserId -> Entity Comment -> DB Bool
userCanDeleteCommentDB user_id (Entity comment_id comment) =
    if commentUser comment /= user_id
        then return False
        else do
          descendants_ids <- fetchCommentAllDescendantsDB comment_id
          if null descendants_ids
              then return True
              else return False

-- | Fetch a User's number of unviewed comments on each WikiPage of a Project.
fetchNumUnviewedCommentsOnProjectWikiPagesDB :: UserId -> ProjectId -> DB (Map WikiPageId Int)
fetchNumUnviewedCommentsOnProjectWikiPagesDB user_id project_id = fmap (M.fromList . map unwrapValues) $
    select $
    from $ \(c `InnerJoin` wp) -> do
    on_ (c ^. CommentDiscussion ==. wp ^. WikiPageDiscussion)
    where_ $
        exprWikiPageOnProject wp project_id &&.
        c ^. CommentId `notIn` exprUserViewedComments user_id
    groupBy (wp ^. WikiPageId)
    let countRows' = countRows :: SqlExpr (Value Int)
    having (countRows' >. val 0)
    return (wp ^. WikiPageId, countRows')

fetchNumUnviewedWikiEditsOnProjectDB :: UserId -> ProjectId -> DB (Map WikiPageId Int)
fetchNumUnviewedWikiEditsOnProjectDB user_id project_id = fmap (M.fromList . map unwrapValues) $
    select $
    from $ \(wp `InnerJoin` we) -> do
    on_ (wp ^. WikiPageId ==. we ^. WikiEditPage)
    where_ $
        exprWikiPageOnProject wp project_id &&.
        we ^. WikiEditId `notIn` exprUserViewedWikiEdits user_id
    groupBy (wp ^. WikiPageId)
    let countRows' = countRows :: SqlExpr (Value Int)
    having (countRows' >. val 0)
    return (wp ^. WikiPageId, countRows')

fetchNumUnreadNotificationsDB :: UserId -> DB Int
fetchNumUnreadNotificationsDB user_id =
    selectCount $
    from $ \(u `InnerJoin` n) -> do
    on_ (u ^. UserId ==. n ^. NotificationTo)
    where_ $
        u ^. UserId ==. val user_id &&.
        n ^. NotificationCreatedTs >=. u ^. UserReadNotifications
