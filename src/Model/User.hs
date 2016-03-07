module Model.User
    ( UserMap
    , UserUpdate (..)
    , ChangePassphrase (..)
    , SetPassphrase (..)
    -- Utility functions
    , anonymousUser
    , deletedUser
    , curUserIsEligibleEstablish
    , updateUserPreview
    , userCanAddTag
    , userIsEligibleEstablish
    , userIsEstablished
    , userIsModerator
    , userIsUnestablished
    , userDisplayName
    -- Database actions
    , deleteFromEmailVerification
    , deleteUserDB
    , eligEstablishUserDB
    , establishUserDB
    , fetchAllUsersDB
    , fetchAllUserProjectInfosDB
    , fetchAllUserRolesDB
    , fetchCurUserRolesDB
    , fetchUserEmail
    , fetchUserEmailVerified
    , fetchUserProjectsAndRolesDB
    , fetchUserRolesDB
    , fetchUserWatchingProjectsDB
    , fetchVerEmail
    , fromEmailVerification
    , updateUserDB
    , updateUserPassphraseDB
    , userHasRoleDB
    , userHasRolesAnyDB
    , userIsAffiliatedWithProjectDB
    , userIsProjectAdminDB
    , userIsProjectModeratorDB
    , userIsProjectTeamMemberDB
    , userIsWatchingProjectDB
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

import Model.User.Internal
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
eligEstablishUserDB :: Text -> UserId -> UserId -> Text -> DB ()
eligEstablishUserDB _honor_pledge establisher_id user_id reason = do
    elig_time <- liftIO getCurrentTime
    let est = EstEligible elig_time reason
    update $ \u -> do
        set u [UserEstablished =. val est]
        where_ (u ^. UserId ==. val user_id)

    insert_ $ ManualEstablishment user_id establisher_id

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

userIsWatchingProjectDB :: UserId -> ProjectId -> DB Bool
userIsWatchingProjectDB user_id project_id =
    maybe False (const True)
        <$> getBy (UniqueUserWatchingProject user_id project_id)
