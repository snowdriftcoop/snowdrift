module Model.User
    ( UserMap
    , UserUpdate (..)
    , ChangePassphrase (..)
    , SetPassphrase (..)
    , NotificationSender (..)
    , NotificationReceiver (..)
    -- Utility functions
    , anonymousUser
    , deletedUser
    , curUserIsEligibleEstablish
    , deleteArchivedNotificationsDB
    , deleteArchivedUserNotificationsDB
    , deleteArchivedProjectNotificationsDB
    , deleteUserNotificationDB
    , deleteProjectNotificationDB
    , deleteNotificationsDB
    , deleteUserNotificationsDB
    , deleteProjectNotificationsDB
    , projectNotificationPref
    , updateUserPreview
    , userCanAddTag
    , userIsEligibleEstablish
    , userIsEstablished
    , userIsModerator
    , userIsUnestablished
    , userDisplayName
    , userNotificationPref
    -- Database actions
    , archiveNotificationsDB
    , archiveUserNotificationsDB
    , archiveProjectNotificationsDB
    , deleteFromEmailVerification
    , deleteUserDB
    , eligEstablishUserDB
    , establishUserDB
    , fetchAllUsersDB
    , fetchAllUserProjectInfosDB
    , fetchAllUserRolesDB
    , fetchCurUserRolesDB
    , fetchNumUnreadNotificationsDB
    , fetchNumUnreadUserNotificationsDB
    , fetchNumUnreadProjectNotificationsDB
    , fetchArchivedUserNotificationsDB
    , fetchArchivedProjectNotificationsDB
    , fetchUserEmail
    , fetchUserEmailVerified
    , fetchUserNotificationsDB
    , fetchProjectNotificationsDB
    , fetchUserNotificationPrefDB
    , fetchProjectNotificationPrefDB
    , fetchUserProjectsAndRolesDB
    , fetchUserRolesDB
    , fetchUsersByUserNotifPrefDB
    , fetchUsersByProjectNotifPrefDB
    , fetchUserWatchingProjectsDB
    , fetchVerEmail
    , fromEmailVerification
    , sendPreferredUserNotificationDB
    , sendPreferredProjectNotificationDB
    , unarchiveNotificationsDB
    , unarchiveUserNotificationsDB
    , unarchiveProjectNotificationsDB
    , updateUserDB
    , updateUserPassphraseDB
    , updateUserNotificationPrefDB
    , updateProjectNotificationPrefDB
    , userHasRoleDB
    , userHasRolesAnyDB
    , userIsAffiliatedWithProjectDB
    , userIsProjectAdminDB
    , userIsProjectModeratorDB
    , userIsProjectTeamMemberDB
    , userIsWatchingProjectDB
    , userReadNotificationsDB
    , userReadVolunteerApplicationsDB
    , userUnwatchProjectDB
    , userWatchProjectDB
    , verifyEmailDB
    -- Unsorted
    , canCurUserMakeEligible
    , canMakeEligible
    ) where

import Import hiding (exists)

import Control.Monad.Trans.Reader (ReaderT)
import Database.Esqueleto.Internal.Language (From)
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Database.Persist as P

import Model.Notification
import Model.User.Internal
            hiding (UserNotificationPref, ProjectNotificationPref)
import WrappedValues

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
curUserIsEligibleEstablish =
    maybe False (userIsEligibleEstablish . entityVal) <$> maybeAuth

-- | Get a User's public display name (defaults to userN if no name has
-- been set).
userDisplayName :: Entity User -> Text
userDisplayName (Entity user_id user) =
    fromMaybe ("user" <> toPathPiece user_id) (userName user)

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

-- | Fetch all users, ordered by descending user id.
fetchAllUsersDB :: DB [Entity User]
fetchAllUsersDB =
      select $
      from $ \user -> do
      orderBy [desc (user ^. UserId)]
      return user

-- | Fetch all users with their "project infos" (mapping from project name and
-- handle to the set of user roles).
fetchAllUserProjectInfosDB :: DB (Map UserId (Map (Text, Text) (Set Role)))
fetchAllUserProjectInfosDB = fmap go query
  where
    go :: [(Value UserId, Value Text, Value Text, Value Role)] -> Map UserId (Map (Text, Text) (Set Role))
    go = M.fromListWith (M.unionWith S.union) . map f
      where
        f :: (Value UserId, Value Text, Value Text, Value Role) -> (UserId, Map (Text, Text) (Set Role))
        f (Value uid, Value name, Value handle, Value role) = (uid, M.singleton (name, handle) (S.singleton role))

    query :: DB [(Value UserId, Value Text, Value Text, Value Role)]
    query =
        select $
        from $ \(user `InnerJoin` role `InnerJoin` project) -> do
        on_ (project ^. ProjectId ==. role ^. ProjectUserRoleProject)
        on_ (user ^. UserId ==. role ^. ProjectUserRoleUser)
        return (user ^. UserId, project ^. ProjectName, project ^. ProjectHandle, role ^. ProjectUserRoleRole)

fetchUserWatchingProjectsDB :: UserId -> DB [Entity Project]
fetchUserWatchingProjectsDB user_id =
    select $ from $ \(uwp,p) -> do
        where_ $ uwp ^. UserWatchingProjectUser    ==. val user_id
             &&. uwp ^. UserWatchingProjectProject ==. p ^. ProjectId

        return p

updateUserDB :: UserId -> UserUpdate -> DB ()
updateUserDB user_id UserUpdate{..} = update $ \u -> do
     set u [ UserName               =. val userUpdateName
           , UserAvatar             =. val userUpdateAvatar
           , UserEmail              =. val userUpdateEmail
           , UserIrcNick            =. val userUpdateIrcNick
           , UserStatement          =. val userUpdateStatement
           , UserBlurb              =. val userUpdateBlurb
           ]
     where_ (u ^. UserId ==. val user_id)

updateUserPassphraseDB :: UserId -> Maybe Text -> Maybe Text -> DB ()
updateUserPassphraseDB user_id hash salt =
    update $ \u -> do
        set u [ UserHash =. val hash
              , UserSalt =. val salt ]
        where_ $ u ^. UserId ==. val user_id

fromEmailVerification
    :: From query expr backend (expr (Entity EmailVerification))
    => UserId -> query ()
fromEmailVerification user_id =
    from $ \ev -> where_ $ ev ^. EmailVerificationUser ==. val user_id

deleteFromEmailVerification :: MonadIO m => UserId -> SqlPersistT m ()
deleteFromEmailVerification user_id =
    delete $ fromEmailVerification user_id

deleteUserDB :: UserId -> DB ()
deleteUserDB user_id = do
    deleteCascade user_id

fetchVerEmail :: Text -> UserId -> DB (Maybe Text)
fetchVerEmail ver_uri user_id = do
    emails <- fmap (map unValue) $
              select $ from $ \ev -> do
                  where_ $ ev ^. EmailVerificationUser ==. val user_id
                       &&. ev ^. EmailVerificationUri  ==. val ver_uri
                  return $ ev ^. EmailVerificationEmail
    if L.null emails
        then return Nothing
        else return $ Just $ L.head emails

verifyEmailDB :: MonadIO m => UserId -> ReaderT SqlBackend m ()
verifyEmailDB user_id = do
    update $ \u -> do
        set u [UserEmail_verified =. val True]
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
    sendPreferredUserNotificationDB
        (Just $ NotificationSender establisher_id)
        (NotificationReceiver user_id)
        NotifEligEstablish
        content
  where
    content :: Markdown
    content = Markdown $ T.unlines
        [ "You are now eligible to become an *established* user."
        , ""
        , "After you [accept the honor pledge](" <> honor_pledge <>
          "), you can comment and take other actions on the site without "
          <> "moderation."
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
    select . distinct $
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
userHasRolesAnyDB roles user_id project_id =
    (or . flip map roles . flip elem) <$> fetchUserRolesDB user_id project_id

-- | Get all Projects this User is affiliated with, along with each Role.
fetchUserProjectsAndRolesDB :: UserId -> DB (Map (Entity Project) (Set Role))
fetchUserProjectsAndRolesDB user_id = fmap buildMap $
    select $
        from $ \(p `InnerJoin` pur) -> do
        on_ (p ^. ProjectId ==.  pur ^. ProjectUserRoleProject)
        where_ (pur ^. ProjectUserRoleUser ==. val user_id)
        return (p, pur ^. ProjectUserRoleRole)
  where
    buildMap :: [(Entity Project, Value Role)]
             -> Map (Entity Project) (Set Role)
    buildMap =
        foldr (\(p, Value r) -> M.insertWith (<>) p (S.singleton r)) mempty

userIsProjectAdminDB :: UserId -> ProjectId -> DB Bool
userIsProjectAdminDB = userHasRoleDB Admin

userIsProjectTeamMemberDB :: UserId -> ProjectId -> DB Bool
userIsProjectTeamMemberDB = userHasRoleDB TeamMember

userIsProjectModeratorDB :: UserId -> ProjectId -> DB Bool
userIsProjectModeratorDB = userHasRoleDB Moderator

-- | A User is affiliated with a Project if they have *any* Role.
userIsAffiliatedWithProjectDB :: UserId -> ProjectId -> DB Bool
userIsAffiliatedWithProjectDB = userHasRolesAnyDB [minBound..maxBound]

-- | Check if the current User can make the given User eligible for
-- establishment. This is True if the current User is a Moderator of any
-- Project, and the given User is Unestablished.
canCurUserMakeEligible :: UserId -> Handler Bool
canCurUserMakeEligible user_id =
    maybeAuthId >>= maybe (return False) (canMakeEligible user_id)

-- | Check if a User (FIRST ARG) can be made eligible by another User
-- (SECOND ARG).
canMakeEligible :: UserId -> UserId -> Handler Bool
canMakeEligible establishee_id establisher_id = do
    (establishee, establisher_is_mod) <- runYDB $ (,)
        <$> get404 establishee_id
        <*> userIsModerator establisher_id
    return $ userIsUnestablished establishee && establisher_is_mod

fetchUserNotificationsDB :: UserId -> DB [Entity UserNotification]
fetchUserNotificationsDB =
    fetchUserNotifs (not_ . (^. UserNotificationArchived))

fetchProjectNotificationsDB :: UserId -> DB [Entity ProjectNotification]
fetchProjectNotificationsDB =
    fetchProjectNotifs (not_ . (^. ProjectNotificationArchived))

fetchArchivedUserNotificationsDB :: UserId -> DB [Entity UserNotification]
fetchArchivedUserNotificationsDB = fetchUserNotifs (^. UserNotificationArchived)

fetchArchivedProjectNotificationsDB :: UserId -> DB [Entity ProjectNotification]
fetchArchivedProjectNotificationsDB =
    fetchProjectNotifs (^. ProjectNotificationArchived)

-- | Abstract fetching archived/unarchived notifications. Unexported.
fetchNotifs :: ( MonadIO m, PersistEntity val, PersistField a
               , PersistEntityBackend val ~ SqlBackend )
            => EntityField val UserId -> EntityField val a
            -> (SqlExpr (Entity val) -> SqlExpr (Value Bool))
            -> UserId -> SqlPersistT m [Entity val]
fetchNotifs notif_to notif_created_ts cond user_id =
    select $ from $ \n -> do
        where_ $ n ^. notif_to ==. val user_id
             &&. cond n
        orderBy [desc $ n ^. notif_created_ts]
        return n

-- | Abstract fetching archived/unarchived user notifications. Unexported.
fetchUserNotifs :: (SqlExpr (Entity UserNotification) -> SqlExpr (Value Bool))
                -> UserId -> DB [Entity UserNotification]
fetchUserNotifs =
    fetchNotifs UserNotificationTo UserNotificationCreatedTs

-- | Abstract fetching archived/unarchived project notifications. Unexported.
fetchProjectNotifs
    :: (SqlExpr (Entity ProjectNotification)
    -> SqlExpr (Value Bool))
    -> UserId
    -> DB [Entity ProjectNotification]
fetchProjectNotifs =
    fetchNotifs ProjectNotificationTo ProjectNotificationCreatedTs

deleteUserNotificationDB :: UserNotificationId -> DB ()
deleteUserNotificationDB = deleteCascade

deleteProjectNotificationDB :: ProjectNotificationId -> DB ()
deleteProjectNotificationDB = deleteCascade

deleteNotificationsDB :: UserId -> DB ()
deleteNotificationsDB user_id = do
    deleteUserNotificationsDB user_id
    deleteProjectNotificationsDB user_id

deleteUserNotificationsDB :: UserId -> DB ()
deleteUserNotificationsDB user_id = do
    notifs <- fetchUserNotificationsDB user_id
    deleteCascadeWhere [UserNotificationId <-. (entityKey <$> notifs)]

deleteProjectNotificationsDB :: UserId -> DB ()
deleteProjectNotificationsDB user_id = do
    notifs <- fetchProjectNotificationsDB user_id
    deleteCascadeWhere [ProjectNotificationId <-. (entityKey <$> notifs)]

deleteArchivedNotificationsDB :: UserId -> DB ()
deleteArchivedNotificationsDB user_id = do
    deleteArchivedUserNotificationsDB user_id
    deleteArchivedProjectNotificationsDB user_id

deleteArchivedUserNotificationsDB :: UserId -> DB ()
deleteArchivedUserNotificationsDB user_id = do
    notifs <- fetchArchivedUserNotificationsDB user_id
    deleteCascadeWhere [UserNotificationId <-. (entityKey <$> notifs)]

deleteArchivedProjectNotificationsDB :: UserId -> DB ()
deleteArchivedProjectNotificationsDB user_id = do
    notifs <- fetchArchivedProjectNotificationsDB user_id
    deleteCascadeWhere [ProjectNotificationId <-. (entityKey <$> notifs)]

archiveNotificationsDB :: UserId -> DB ()
archiveNotificationsDB user_id = do
    archiveUserNotificationsDB user_id
    archiveProjectNotificationsDB user_id

archiveUserNotificationsDB :: UserId -> DB ()
archiveUserNotificationsDB user_id = do
    notifs <- fetchUserNotificationsDB user_id
    forM_ notifs $ \(Entity notif_id _) ->
        archiveUserNotificationDB notif_id

archiveProjectNotificationsDB :: UserId -> DB ()
archiveProjectNotificationsDB user_id = do
    notifs <- fetchProjectNotificationsDB user_id
    forM_ notifs $ \(Entity notif_id _) ->
        archiveProjectNotificationDB notif_id

unarchiveNotificationsDB :: UserId -> DB ()
unarchiveNotificationsDB user_id = do
    unarchiveUserNotificationsDB user_id
    unarchiveProjectNotificationsDB user_id

unarchiveUserNotificationsDB :: UserId -> DB ()
unarchiveUserNotificationsDB user_id = do
    notifs <- fetchArchivedUserNotificationsDB user_id
    forM_ notifs $ \(Entity notif_id _) ->
        unarchiveUserNotificationDB notif_id

unarchiveProjectNotificationsDB :: UserId -> DB ()
unarchiveProjectNotificationsDB user_id = do
    notifs <- fetchArchivedProjectNotificationsDB user_id
    forM_ notifs $ \(Entity notif_id _) ->
        unarchiveProjectNotificationDB notif_id

deleteUserNotifPrefs :: UserId -> UserNotificationType -> DB ()
deleteUserNotifPrefs user_id notif_type =
    delete $ from $ \unp ->
        where_ $ unp ^. UserNotificationPrefUser ==. val user_id
             &&. unp ^. UserNotificationPrefType ==. val notif_type

deleteProjectNotifPrefs :: UserId -> ProjectId -> ProjectNotificationType
                        -> DB ()
deleteProjectNotifPrefs user_id project_id notif_type =
    delete $ from $ \pnp ->
        where_ $ pnp ^. ProjectNotificationPrefUser    ==. val user_id
             &&. pnp ^. ProjectNotificationPrefProject ==. val project_id
             &&. pnp ^. ProjectNotificationPrefType    ==. val notif_type

updateUserNotifPrefs :: UserId -> UserNotificationType
                     -> UserNotificationDelivery -> DB ()
updateUserNotifPrefs user_id notif_type notif_deliv = do
    deleteUserNotifPrefs user_id notif_type
    insert_ $ UserNotificationPref user_id notif_type notif_deliv

updateProjectNotifPrefs :: UserId -> ProjectId -> ProjectNotificationType
                        -> ProjectNotificationDelivery -> DB ()
updateProjectNotifPrefs user_id project_id notif_type notif_deliv = do
    deleteProjectNotifPrefs user_id project_id notif_type
    insert_ $ ProjectNotificationPref user_id project_id notif_type notif_deliv

updateUserNotificationPrefDB :: UserId -> UserNotificationType
                             -> Maybe UserNotificationDelivery -> DB ()
updateUserNotificationPrefDB user_id notif_type =
    maybe (deleteUserNotifPrefs user_id notif_type)
          (updateUserNotifPrefs user_id notif_type)

updateProjectNotificationPrefDB :: UserId -> ProjectId
                                -> ProjectNotificationType
                                -> Maybe ProjectNotificationDelivery -> DB ()
updateProjectNotificationPrefDB user_id project_id notif_type =
    maybe (deleteProjectNotifPrefs user_id project_id notif_type)
          (updateProjectNotifPrefs user_id project_id notif_type)

insertDefaultProjectNotifPrefs :: UserId -> ProjectId -> DB ()
insertDefaultProjectNotifPrefs user_id project_id =
    void $ insertMany $ uncurry (ProjectNotificationPref user_id project_id) <$>
        [ (NotifBlogPost,      ProjectNotifDeliverWebsiteAndEmail)
        , (NotifNewPledge,     ProjectNotifDeliverWebsite)
        , (NotifUpdatedPledge, ProjectNotifDeliverWebsite)
        , (NotifDeletedPledge, ProjectNotifDeliverWebsite)
        ]

userWatchProjectDB :: UserId -> ProjectId -> DB ()
userWatchProjectDB user_id project_id = do
    void $ insertUnique $ UserWatchingProject user_id project_id
    exists' <- fmap (>0) $
        P.count [ProjectNotificationPrefUser P.==. user_id
                ,ProjectNotificationPrefProject P.==. project_id]
    unless exists' $
        insertDefaultProjectNotifPrefs user_id project_id

userUnwatchProjectDB :: UserId -> ProjectId -> DB ()
userUnwatchProjectDB user_id project_id =
    deleteBy $ UniqueUserWatchingProject user_id project_id

userIsWatchingProjectDB :: UserId -> ProjectId -> DB Bool
userIsWatchingProjectDB user_id project_id =
    maybe False (const True)
        <$> getBy (UniqueUserWatchingProject user_id project_id)

userReadNotificationsDB :: UserId -> DB ()
userReadNotificationsDB user_id = liftIO getCurrentTime >>= \now ->
    update $ \u -> do
    set u [UserReadNotifications =. val now]
    where_ (u ^. UserId ==. val user_id)

-- | Update this User's read applications timestamp.
userReadVolunteerApplicationsDB :: UserId -> DB ()
userReadVolunteerApplicationsDB user_id = liftIO getCurrentTime >>= \now ->
    update $ \u -> do
    set u [UserReadApplications =. val now]
    where_ (u ^. UserId ==. val user_id)

fetchNumUnreadNotificationsDB :: UserId -> DB Int
fetchNumUnreadNotificationsDB user_id = (+)
    <$> fetchNumUnreadUserNotificationsDB user_id
    <*> fetchNumUnreadProjectNotificationsDB user_id

-- Internal helper.
fetchNumUnreadNotifications :: ( PersistEntity val
                               , PersistEntityBackend val ~ SqlBackend )
                            => EntityField val UserId
                            -> EntityField val UTCTime
                            -> UserId
                            -> DB Int
fetchNumUnreadNotifications notif_to notif_created_ts user_id =
    -- countRows returns at least element. I think.
    fmap (unValue . L.head)
         (select $
          from $ \(u `InnerJoin` n) -> do
          on_ (u ^. UserId ==. n ^. notif_to)
          where_ $ u ^. UserId ==. val user_id
             &&. n ^. notif_created_ts >=. u ^. UserReadNotifications
          return countRows)

fetchNumUnreadUserNotificationsDB :: UserId -> DB Int
fetchNumUnreadUserNotificationsDB =
    fetchNumUnreadNotifications
        UserNotificationTo UserNotificationCreatedTs

fetchNumUnreadProjectNotificationsDB :: UserId -> DB Int
fetchNumUnreadProjectNotificationsDB =
    fetchNumUnreadNotifications
        ProjectNotificationTo ProjectNotificationCreatedTs
