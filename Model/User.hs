module Model.User
    ( UserMap
    -- Utility functions
    , anonymousUser
    , curUserIsEligibleEstablish
    , updateUserPreview
    , userCanAddTag
    , userCanCloseComment
    , userCanEditComment
    , userCanEditWikiPage
    , userIsEligibleEstablish
    , userIsEstablished
    , userIsUnestablished
    , userDisplayName
    -- Database actions
    , buildAnnotatedCommentTagsDB
    , eligEstablishUserDB
    , establishUserDB
    , fetchAllUserRolesDB
    , fetchCurUserRolesDB
    , fetchNumUnreadNotificationsDB
    , fetchNumUnviewedCommentsOnProjectWikiPagesDB
    , fetchNumUnviewedWikiEditsOnProjectDB
    , fetchUserArchivedNotificationsDB
    , fetchUserNotificationsDB
    , fetchUserNotificationPrefDB
    , fetchUserProjectsAndRolesDB
    , fetchUserRolesDB
    , fetchUsersInDB
    , updateUserDB
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
    -- Unsorted
    , canCurUserMakeEligible
    , canMakeEligible
    ) where

import Import


import Model.Comment
import Model.Comment.Sql
import Model.Notification
import Model.Project.Sql
import Model.Tag
import Model.User.Internal
import Model.User.Sql
import Model.Wiki.Sql

import           Data.List      (sortBy)
import qualified Data.Map       as M
import qualified Data.Set       as S
import qualified Data.Text      as T
import           Yesod.Markdown (Markdown(..))

anonymousUser :: UserId
anonymousUser = Key $ PersistInt64 (-1)

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

-- This stuff sets notification prefs (although might be only half-way
-- complete):
    delete $
     from $ \ump -> do
     where_ (ump ^. UserNotificationPrefUser ==. val user_id)
--  let new_prefs = map (uncurry (UserNotificationPref user_id)) userUpdateNotificationPreferences
--  void (insertMany new_prefs)

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
eligEstablishUserDB :: UserId -> UserId -> Text -> SDB ()
eligEstablishUserDB establisher_id user_id reason = do
    elig_time <- liftIO getCurrentTime
    let est = EstEligible elig_time reason
    lift $
        update $ \u -> do
        set u [UserEstablished =. val est]
        where_ (u ^. UserId ==. val user_id)

    lift $ insert_ $ ManualEstablishment user_id establisher_id

    snowdrift_id <- lift getSnowdriftId
    sendNotificationDB_ NotifEligEstablish user_id (Just snowdrift_id) content
  where
    content :: Markdown
    content = Markdown $ T.unlines
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

-- | Does this User have any of these Roles in this Project?
userHasRolesAnyDB :: [Role] -> UserId -> ProjectId -> DB Bool
userHasRolesAnyDB roles user_id project_id = (or . flip map roles . flip elem) <$> fetchUserRolesDB user_id project_id

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
        <*> (elem Moderator <$> fetchAllUserRolesDB establisher_id)
    return $ userIsUnestablished establishee && establisher_is_mod

-- | How does this User prefer notifications of a certain type to be delivered (if at all)?
-- listToMaybe is appropriate here due to UniqueUserNotificationPref (list returned will
-- either be [] or [Value delivery])
fetchUserNotificationPrefDB :: UserId -> NotificationType -> DB (Maybe NotificationDelivery)
fetchUserNotificationPrefDB user_id notif_type = fmap (fmap unValue . listToMaybe) $
    select $
    from $ \unp -> do
    where_ $
        unp ^. UserNotificationPrefUser ==. val user_id &&.
        unp ^. UserNotificationPrefType ==. val notif_type
    return (unp ^. UserNotificationPrefDelivery)

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

userWatchProjectDB :: UserId -> ProjectId -> DB ()
userWatchProjectDB user_id project_id = void (insertUnique (UserWatchingProject user_id project_id))

userUnwatchProjectDB :: UserId -> ProjectId -> DB ()
userUnwatchProjectDB user_id project_id = do
    delete_watching
    delete_comment_views
    delete_wiki_edit_views
  where
    delete_watching = deleteBy (UniqueUserWatchingProject user_id project_id)

    delete_comment_views = delete_wiki_page_comment_views

    delete_wiki_page_comment_views =
        delete $
        from $ \vc ->
        where_ (vc ^. ViewCommentComment `in_` subList_select (querProjectCommentsOnWikiPagesDB project_id (Just user_id)))

    delete_wiki_edit_views =
        delete $
        from $ \vwe ->
        where_ (vwe ^. ViewWikiEditEdit `in_` (subList_select (querProjectWikiEdits project_id)))

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
fetchNumUnreadNotificationsDB user_id = fmap (\[Value n] -> n) $
    select $
    from $ \(u `InnerJoin` n) -> do
    on_ (u ^. UserId ==. n ^. NotificationTo)
    where_ $
        u ^. UserId ==. val user_id &&.
        n ^. NotificationCreatedTs >=. u ^. UserReadNotifications
    return countRows

-- | Annotate a [CommentTag]. Returns a Map CommentId [AnnotatedTag] so this
-- function can be called with multiple Comments' CommentTags. If all
-- [CommentTag] are of the same comment, that's fine -- the returned map will
-- only have one key.
--
-- The [AnnotatedTag] value is left unsorted, but the [(Entity User, Int)] within each
-- AnnotatedTag is sorted by ascending username.
buildAnnotatedCommentTagsDB :: Maybe UserId -> [CommentTag] -> DB (Map CommentId [AnnotatedTag])
buildAnnotatedCommentTagsDB muser_id comment_tags =  do
    user_map <- entitiesMap <$> fetchUsersInDB (map commentTagUser comment_tags)
    tag_map <- entitiesMap <$> fetchTagsInDB (map commentTagTag comment_tags)
    -- TODO(mitchell): cached
    tag_colors <- maybe fetchDefaultTagColorsDB fetchTagColorsDB muser_id

    let f :: [CommentTag] -> Map CommentId [AnnotatedTag]
        f = M.mapWithKey (map . i) . M.map h . g

        -- Pair each CommentTag with its CommentId, then collect CommentTags back up,
        -- grouped by their CommentIds.
        g :: [CommentTag] -> Map CommentId [CommentTag]
        g = M.fromListWith (++) . map (commentTagComment &&& return)

        -- Group each CommentTag by TagId, combining Users' votes.
        h :: [CommentTag] -> [(TagId, [(Entity User, Int)])]
        h = M.toList . foldr step mempty
          where
            step :: CommentTag -> Map TagId [(Entity User, Int)] -> Map TagId [(Entity User, Int)]
            step (CommentTag _ tag_id user_id n) =
                M.insertWith (++) tag_id [(Entity user_id (user_map M.! user_id), n)]

        -- Construct an AnnotatedTag given all relevant info.
        i :: CommentId -> (TagId, [(Entity User, Int)]) -> AnnotatedTag
        i comment_id (tag_id, user_votes) =
          AnnotatedTag
            (Entity tag_id (tag_map M.! tag_id))
            (CommentTagR comment_id tag_id)
            (M.findWithDefault 0x77AADD tag_id tag_colors)
            (sortBy (compare `on` (userName . entityVal . fst)) user_votes)

    return (f comment_tags)
