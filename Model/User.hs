module Model.User
    ( UserMap
    , UserUpdate(..)
    , applyUserUpdate
    , canCurUserMakeEligible
    , canMakeEligible
    , eligEstablishUser
    , establishUser
    , getAllRoles
    , getCurUserRoles
    , getProjectsAndRoles
    , getRoles
    , getUsersIn
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

import qualified Data.Map       as M
import qualified Data.Set       as S
import qualified Data.Text      as T
import           Yesod.Markdown (Markdown(..))

type UserMap = Map UserId User

data UserUpdate =
    UserUpdate
        { userUpdateName :: Maybe Text
        , userUpdateAvatar :: Maybe Text
        , userUpdateIrcNick :: Maybe Text
        , userUpdateBlurb :: Maybe Markdown
        , userUpdateStatement :: Maybe Markdown
        }

getUsersIn :: [UserId] -> YesodDB App [Entity User]
getUsersIn user_ids = selectList [UserId <-. user_ids] []

updateUser :: UserId -> UserUpdate -> YesodDB App ()
updateUser user_id user_update = update $ \ user -> do
        set user $ catMaybes
            [ (UserName =.) . val . Just <$> userUpdateName user_update
            , (UserAvatar =.) . val . Just <$> userUpdateAvatar user_update
            , (UserIrcNick =.) . val . Just <$> userUpdateIrcNick user_update
            , (UserStatement =.) . val . Just <$> userUpdateStatement user_update
            , (UserBlurb =.) . val . Just <$> userUpdateBlurb user_update
            ]
        where_ ( user ^. UserId ==. val user_id )

applyUserUpdate :: User -> UserUpdate -> User
applyUserUpdate user user_update = user
        { userName = fromMaybe (userName user) $ Just <$> userUpdateName user_update
        , userAvatar = fromMaybe (userAvatar user) $ Just <$> userUpdateAvatar user_update
        , userIrcNick = fromMaybe (userIrcNick user) $ Just <$> userUpdateIrcNick user_update
        , userStatement = fromMaybe (userStatement user) $ Just <$> userUpdateStatement user_update
        , userBlurb = fromMaybe (userBlurb user) $ Just <$> userUpdateBlurb user_update
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
-- eligibility.
establishUser :: UserId -> UTCTime -> Text -> YesodDB App ()
establishUser user_id elig_time reason = do
    est_time <- liftIO getCurrentTime
    let est = EstEstablished elig_time est_time reason
    update $ \u -> do
        set u [ UserEstablished =. val est ]
        where_ (u ^. UserId ==. val user_id)

-- | Make a user eligible for establishment. Put a message in their inbox
-- instructing them to read and accept the honor pledge.
eligEstablishUser :: UserId -> UserId -> Text -> YesodDB App ()
eligEstablishUser establisher_id user_id reason = do
    elig_time <- liftIO getCurrentTime
    let est = EstEligible elig_time reason
    update $ \u -> do
        set u [ UserEstablished =. val est ]
        where_ (u ^. UserId ==. val user_id)

    insert_ $ ManualEstablishment user_id establisher_id

    snowdrift_id <- getSnowdriftId
    insert_ $ Message (Just snowdrift_id) elig_time Nothing (Just user_id) message_text True
  where
    message_text :: Markdown
    message_text = Markdown $ T.unlines
        -- "Because" <> reason <> ","
        -- That reason could be added when we find a good way to fit it in,
        -- but for now it is just stored in the database
        [ "You are now eligible to become an *established* user."
        , ""
        , "After you [accept the honor pledge](/honor-pledge), you can comment and take other actions on the site without moderation."
        ]

-- | Get a User's Roles in a Project.
getRoles :: UserId -> ProjectId -> YesodDB App [Role]
getRoles user_id project_id = fmap (map unValue) $
    select $
        from $ \r -> do
        where_ (r ^. ProjectUserRoleProject ==. val project_id &&.
                r ^. ProjectUserRoleUser ==. val user_id)
        return $ r ^. ProjectUserRoleRole

-- | Get all of a User's Roles, across all Projects.
getAllRoles :: UserId -> YesodDB App [Role]
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
hasRole :: Role -> UserId -> ProjectId -> YesodDB App Bool
hasRole role user_id = fmap (elem role) . getRoles user_id

-- | Get all Projects this User is affiliated with, along with each Role.
getProjectsAndRoles :: UserId -> YesodDB App (Map (Entity Project) (Set Role))
getProjectsAndRoles user_id = fmap buildMap $
    select $
        from $ \(p `InnerJoin` pur) -> do
        on_ (p ^. ProjectId ==.  pur ^. ProjectUserRoleProject)
        where_ (pur ^. ProjectUserRoleUser ==. val user_id)
        return (p, pur ^. ProjectUserRoleRole)
  where
    buildMap :: [(Entity Project, Value Role)] -> Map (Entity Project) (Set Role)
    buildMap = foldr (\(p, Value r) -> M.insertWith (<>) p (S.singleton r)) mempty

isProjectAdmin' :: UserId -> ProjectId -> YesodDB App Bool
isProjectAdmin' = hasRole Admin

isProjectTeamMember' :: UserId -> ProjectId -> YesodDB App Bool
isProjectTeamMember' = hasRole TeamMember

isProjectModerator' :: UserId -> ProjectId -> YesodDB App Bool
isProjectModerator' = hasRole Moderator

isProjectAdmin :: Text -> UserId -> YesodDB App Bool
isProjectAdmin project_handle user_id =
    fmap (not . null) $ select $ from $ \ (pur `InnerJoin` p) -> do
        on_ $ pur ^. ProjectUserRoleProject ==. p ^. ProjectId
        where_ $ p ^. ProjectHandle ==. val project_handle
            &&. pur ^. ProjectUserRoleUser ==. val user_id
            &&. pur ^. ProjectUserRoleRole ==. val Admin
        limit 1
        return ()

isProjectTeamMember :: Text -> UserId -> YesodDB App Bool
isProjectTeamMember project_handle user_id =
    fmap (not . null) $ select $ from $ \ (pur `InnerJoin` p) -> do
        on_ $ pur ^. ProjectUserRoleProject ==. p ^. ProjectId
        where_ $ p ^. ProjectHandle ==. val project_handle
            &&. pur ^. ProjectUserRoleUser ==. val user_id
            &&. pur ^. ProjectUserRoleRole ==. val TeamMember
        limit 1
        return ()

isProjectModerator :: Text -> UserId -> YesodDB App Bool
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
    maybeAuthId >>= maybe (return False) (runDB . isProjectModerator project_handle)

isProjectAffiliated :: Text -> UserId -> YesodDB App Bool
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
    (establishee, establisher_is_mod) <- runDB $ (,)
        <$> get404 establishee_id
        <*> (elem Moderator <$> getAllRoles establisher_id)
    return $ isUnestablished establishee && establisher_is_mod
