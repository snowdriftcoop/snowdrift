module Model.User
    -- Types
    ( UserMap
    , UserUpdate(..)
    -- Utility functions
    , curUserIsEligibleEstablish
    , updateUserPreview
    , userCanCloseComment
    , userCanEditComment
    , userCanEditWikiPage
    , userIsEligibleEstablish
    , userIsEstablished
    , userIsUnestablished
    , userPrintName
    -- Database actions
    , eligEstablishUserDB
    , establishUserDB
    , fetchAllUserRolesDB
    , fetchCurUserRolesDB
    , fetchUserMessagePrefDB
    , fetchUserProjectsAndRolesDB
    , fetchUserRolesDB
    , fetchUsersInDB
    , updateUserDB
    , userCanDeleteCommentDB
    , userIsProjectAdminDB'
    , userIsProjectModeratorDB'
    , userIsProjectTeamMemberDB'
    , userUnwatchProjectDB
    , userViewCommentsDB
    , userWatchingProjectDB
    , userWatchProjectDB
    -- Unsorted
    , canCurUserMakeEligible
    , canMakeEligible
    , isCurUserProjectModerator
    , isProjectAdmin
    , isProjectAffiliated
    , isProjectModerator
    , isProjectTeamMember
    ) where

import Import


import Model.Comment
import Model.Comment.Sql
import Model.Message
import Model.Project.Sql

import qualified Data.Map       as M
import qualified Data.Set       as S
import qualified Data.Text      as T
import           Yesod.Markdown (Markdown(..))

--------------------------------------------------------------------------------
-- Types

type UserMap = Map UserId User

data UserUpdate =
    UserUpdate
        { userUpdateName               :: Maybe Text
        , userUpdateAvatar             :: Maybe Text
        , userUpdateIrcNick            :: Maybe Text
        , userUpdateBlurb              :: Maybe Markdown
        , userUpdateStatement          :: Maybe Markdown
        , userUpdateMessagePreferences :: [(MessageType, MessageDelivery)]
        }

--------------------------------------------------------------------------------
-- Utility functions

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

-- | Is the user unestablished?
userIsUnestablished :: User -> Bool
userIsUnestablished = estIsUnestablished . userEstablished

-- | Is the current user eligible for establishment?
curUserIsEligibleEstablish :: Handler Bool
curUserIsEligibleEstablish = maybe False (userIsEligibleEstablish . entityVal) <$> maybeAuth

-- | Get a User's public display name (defaults to userN if no name has been set).
userPrintName :: Entity User -> Text
userPrintName (Entity user_id user) = fromMaybe ("user" <> toPathPiece user_id) (userName user)

-- | Apply a UserUpdate in memory, for preview. For this reason,
-- userUpdateMessagePreferences doesn't need to be touched.
updateUserPreview :: UserUpdate -> User -> User
updateUserPreview UserUpdate{..} user = user
    { userName               = userUpdateName
    , userAvatar             = userUpdateAvatar
    , userIrcNick            = userUpdateIrcNick
    , userStatement          = userUpdateStatement
    , userBlurb              = userUpdateBlurb
    }

--------------------------------------------------------------------------------
-- Database functions

fetchUsersInDB :: [UserId] -> DB [Entity User]
fetchUsersInDB user_ids = selectList [UserId <-. user_ids] []

updateUserDB :: UserId -> UserUpdate -> DB ()
updateUserDB user_id UserUpdate{..} = do
    update $ \u -> do
     set u $ [ UserName               =. val userUpdateName
             , UserAvatar             =. val userUpdateAvatar
             , UserIrcNick            =. val userUpdateIrcNick
             , UserStatement          =. val userUpdateStatement
             , UserBlurb              =. val userUpdateBlurb
             ]
     where_ (u ^. UserId ==. val user_id)

    delete $
     from $ \ump -> do
     where_ (ump ^. UserId ==. val user_id)

    let new_prefs = map (uncurry (UserMessagePref user_id)) userUpdateMessagePreferences
    void (insertMany new_prefs)

-- | Establish a user, given their eligible-timestamp and reason for
-- eligibility. Mark all unmoderated comments of theirs as moderated.
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
            set c [ CommentModeratedTs =. just (val est_time)
                  , CommentModeratedBy =. just (val user_id)
                  ]
            where_ $
                c ^. CommentUser ==. val user_id &&.
                exprUnapproved c

-- | Make a user eligible for establishment. Put a message in their inbox
-- instructing them to read and accept the honor pledge.
eligEstablishUserDB :: UserId -> UserId -> Text -> SDB ()
eligEstablishUserDB establisher_id user_id reason = do
    elig_time <- liftIO getCurrentTime
    let est = EstEligible elig_time reason
    lift $
        update $ \u -> do
        set u [ UserEstablished =. val est ]
        where_ (u ^. UserId ==. val user_id)

    lift $ insert_ $ ManualEstablishment user_id establisher_id

    snowdrift_id <- lift getSnowdriftId
    insertMessage_ MessageDirect (Just snowdrift_id) Nothing (Just user_id) message_text True
  where
    message_text :: Markdown
    message_text = Markdown $ T.unlines
        [ "You are now eligible to become an *established* user."
        , ""
        , "After you [accept the honor pledge](/honor-pledge), you can comment and take other actions on the site without moderation."
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

userIsProjectAdminDB' :: UserId -> ProjectId -> DB Bool
userIsProjectAdminDB' = userHasRoleDB Admin

userIsProjectTeamMemberDB' :: UserId -> ProjectId -> DB Bool
userIsProjectTeamMemberDB' = userHasRoleDB TeamMember

userIsProjectModeratorDB' :: UserId -> ProjectId -> DB Bool
userIsProjectModeratorDB' = userHasRoleDB Moderator

isProjectAdmin :: Text -> UserId -> DB Bool
isProjectAdmin project_handle user_id =
    fmap (not . null) $ select $ from $ \ (pur `InnerJoin` p) -> do
        on_ $ pur ^. ProjectUserRoleProject ==. p ^. ProjectId
        where_ $ p ^. ProjectHandle ==. val project_handle
            &&. pur ^. ProjectUserRoleUser ==. val user_id
            &&. pur ^. ProjectUserRoleRole ==. val Admin
        limit 1
        return ()

isProjectTeamMember :: Text -> UserId -> DB Bool
isProjectTeamMember project_handle user_id =
    fmap (not . null) $ select $ from $ \ (pur `InnerJoin` p) -> do
        on_ $ pur ^. ProjectUserRoleProject ==. p ^. ProjectId
        where_ $ p ^. ProjectHandle ==. val project_handle
            &&. pur ^. ProjectUserRoleUser ==. val user_id
            &&. pur ^. ProjectUserRoleRole ==. val TeamMember
        limit 1
        return ()

isProjectModerator :: Text -> UserId -> DB Bool
isProjectModerator project_handle user_id =
    fmap (not . null) $ select $ from $ \ (pur `InnerJoin` p) -> do
        on_ $ pur ^. ProjectUserRoleProject ==. p ^. ProjectId
        where_ $ p ^. ProjectHandle ==. val project_handle
            &&. pur ^. ProjectUserRoleUser ==. val user_id
            &&. pur ^. ProjectUserRoleRole ==. val Moderator
        limit 1
        return ()

isCurUserProjectModerator :: Text -> Handler Bool
isCurUserProjectModerator project_handle =
    maybeAuthId >>= maybe (return False) (runYDB . isProjectModerator project_handle)

isProjectAffiliated :: Text -> UserId -> DB Bool
isProjectAffiliated project_handle user_id =
    fmap (not . null) $ select $ from $ \ (pur `InnerJoin` p) -> do
        on_ $ pur ^. ProjectUserRoleProject ==. p ^. ProjectId
        where_ $ p ^. ProjectHandle ==. val project_handle
            &&. pur ^. ProjectUserRoleUser ==. val user_id
        limit 1
        return ()

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
        <*> (elem Moderator <$> fetchAllUserRolesDB establisher_id)
    return $ userIsUnestablished establishee && establisher_is_mod

-- | How does this User prefer messages of a certain type to be delivered (if at all)?
-- listToMaybe is appropriate here due to UniqueUserMessagePref (list returned will
-- either be [] or [Value delivery])
fetchUserMessagePrefDB :: UserId -> MessageType -> DB (Maybe MessageDelivery)
fetchUserMessagePrefDB user_id msg_type = fmap (fmap unValue . listToMaybe) $
    select $
    from $ \ump -> do
    where_ $
        ump ^. UserMessagePrefUser ==. val user_id &&.
        ump ^. UserMessagePrefType ==. val msg_type
    return (ump ^. UserMessagePrefDelivery)

userWatchProjectDB :: UserId -> ProjectId -> DB ()
userWatchProjectDB user_id project_id = void (insertUnique (UserWatchingProject user_id project_id))

userUnwatchProjectDB :: UserId -> ProjectId -> DB ()
userUnwatchProjectDB user_id project_id = delete_watching >> delete_views
  where
    delete_watching = deleteBy (UniqueUserWatchingProject user_id project_id)
    delete_views =
        delete $
        from $ \vc ->
        where_ (vc ^. ViewCommentComment `in_` (subList_select (querProjectCommentIdsPostedOnWikiPagesDB project_id)))

userWatchingProjectDB :: UserId -> ProjectId -> DB Bool
userWatchingProjectDB user_id project_id = maybe (False) (const True) <$> getBy (UniqueUserWatchingProject user_id project_id)

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

userCanDeleteCommentDB :: UserId -> Entity Comment -> DB Bool
userCanDeleteCommentDB user_id (Entity comment_id comment) = do
    if commentUser comment /= user_id
        then return False
        else do
          descendants_ids <- fetchCommentDescendantsIdsDB comment_id
          if null descendants_ids
              then return True
              else return False
