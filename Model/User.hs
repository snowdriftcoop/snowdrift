module Model.User
    ( UserMap
    , UserUpdate(..)
    , applyUserUpdate
    , canCurUserMakeEligible
    , canMakeEligible
    , eligEstablishUser
    , establishUser
    , fetchUserMessagePrefDB
    , fetchUsersInDB
    -- TODO(mitchell): consistent naming scheme
    , getAllRoles
    , getCurUserRoles
    , getProjectsAndRoles
    , getRoles
    , hasRole
    , isCurUserEligibleEstablish
    , isCurUserProjectModerator
    , isEligibleEstablish
    , isEstablished
    , isProjectAdmin
    , isProjectAdmin'
    , isProjectAffiliated
    , isProjectModerator
    , isProjectModerator'
    , isProjectTeamMember
    , isProjectTeamMember'
    , updateUser
    , userPrintName
    , userWidget
    ) where

import Import

import Model.Comment.Sql
import Model.Message

import qualified Data.Map       as M
import qualified Data.Set       as S
import qualified Data.Text      as T
import           Yesod.Markdown (Markdown(..))

type UserMap = Map UserId User

data UserUpdate =
    UserUpdate
        { userUpdateName               :: Maybe Text
        , userUpdateAvatar             :: Maybe Text
        , userUpdateIrcNick            :: Maybe Text
        , userUpdateBlurb              :: Maybe Markdown
        , userUpdateStatement          :: Maybe Markdown
        -- , userUpdateMessagePreferences :: Maybe [MessagePreference]
        }

fetchUsersInDB :: [UserId] -> DB [Entity User]
fetchUsersInDB user_ids = selectList [UserId <-. user_ids] []

updateUser :: UserId -> UserUpdate -> DB ()
updateUser user_id UserUpdate{..} =
    update $ \u -> do
    set u $ [ UserName               =. val userUpdateName
            , UserAvatar             =. val userUpdateAvatar
            , UserIrcNick            =. val userUpdateIrcNick
            , UserStatement          =. val userUpdateStatement
            , UserBlurb              =. val userUpdateBlurb
            -- , UserMessagePreferences =. val (fromMaybe [] userUpdateMessagePreferences)
            ]
    where_ (u ^. UserId ==. val user_id)

applyUserUpdate :: User -> UserUpdate -> User
applyUserUpdate user UserUpdate{..} = user
    { userName               = userUpdateName
    , userAvatar             = userUpdateAvatar
    , userIrcNick            = userUpdateIrcNick
    , userStatement          = userUpdateStatement
    , userBlurb              = userUpdateBlurb
    -- , userMessagePreferences = fromMaybe [] userUpdateMessagePreferences
    }

userPrintName :: Entity User -> Text
userPrintName (Entity user_id user) = fromMaybe ("user" <> toPathPiece user_id) (userName user)

userWidget :: UserId -> Widget
userWidget user_id = do
    maybe_user <- handlerToWidget $ runDB $ get user_id
    case maybe_user of
        Nothing -> [whamlet|deleted user|]
        Just user ->
            [whamlet|
                <a href=@{UserR user_id}>
                    #{userPrintName (Entity user_id user)}
            |]

isEstablished :: User -> Bool
isEstablished = estIsEstablished . userEstablished

isEligibleEstablish :: User -> Bool
isEligibleEstablish = estIsEligible . userEstablished

isUnestablished :: User -> Bool
isUnestablished = estIsUnestablished . userEstablished

isCurUserEligibleEstablish :: Handler Bool
isCurUserEligibleEstablish = maybe False (isEligibleEstablish . entityVal) <$> maybeAuth

-- | Establish a user, given their eligible-timestamp and reason for
-- eligibility. Mark all unmoderated comments of theirs as moderated.
establishUser :: UserId -> UTCTime -> Text -> DB ()
establishUser user_id elig_time reason = do
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
eligEstablishUser :: UserId -> UserId -> Text -> SDB ()
eligEstablishUser establisher_id user_id reason = do
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
getRoles :: UserId -> ProjectId -> DB [Role]
getRoles user_id project_id = fmap (map unValue) $
    select $
        from $ \r -> do
        where_ (r ^. ProjectUserRoleProject ==. val project_id &&.
                r ^. ProjectUserRoleUser ==. val user_id)
        return $ r ^. ProjectUserRoleRole

-- | Get all of a User's Roles, across all Projects.
getAllRoles :: UserId -> DB [Role]
getAllRoles user_id = fmap unwrapValues $
    selectDistinct $
        from $ \pur -> do
        where_ (pur ^. ProjectUserRoleUser ==. val user_id)
        return (pur ^. ProjectUserRoleRole)

-- | Get the current User's Roles in a Project.
getCurUserRoles :: ProjectId -> Handler [Role]
getCurUserRoles project_id = maybeAuthId >>= \case
    Nothing -> return []
    Just user_id -> runDB $ getRoles user_id project_id

-- | Does this User have this Role in this Project?
hasRole :: Role -> UserId -> ProjectId -> DB Bool
hasRole role user_id = fmap (elem role) . getRoles user_id

-- | Get all Projects this User is affiliated with, along with each Role.
getProjectsAndRoles :: UserId -> DB (Map (Entity Project) (Set Role))
getProjectsAndRoles user_id = fmap buildMap $
    select $
        from $ \(p `InnerJoin` pur) -> do
        on_ (p ^. ProjectId ==.  pur ^. ProjectUserRoleProject)
        where_ (pur ^. ProjectUserRoleUser ==. val user_id)
        return (p, pur ^. ProjectUserRoleRole)
  where
    buildMap :: [(Entity Project, Value Role)] -> Map (Entity Project) (Set Role)
    buildMap = foldr (\(p, Value r) -> M.insertWith (<>) p (S.singleton r)) mempty

isProjectAdmin' :: UserId -> ProjectId -> DB Bool
isProjectAdmin' = hasRole Admin

isProjectTeamMember' :: UserId -> ProjectId -> DB Bool
isProjectTeamMember' = hasRole TeamMember

isProjectModerator' :: UserId -> ProjectId -> DB Bool
isProjectModerator' = hasRole Moderator

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
        <*> (elem Moderator <$> getAllRoles establisher_id)
    return $ isUnestablished establishee && establisher_is_mod

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
