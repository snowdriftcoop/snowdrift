module Handler.User where

import Import

import           Model.Role
import           Model.User
import           Widgets.Preview
import           View.User

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

getUserR :: UserId -> Handler Html
getUserR user_id = do
    mviewer_id <- maybeAuthId

    user <- runYDB $ get404 user_id

    projects_and_roles <- runDB (fetchUserProjectsAndRolesDB user_id)

    defaultLayout $ do
        setTitle . toHtml $ "User Profile - " <> userPrintName (Entity user_id user) <> " | Snowdrift.coop"
        renderUser mviewer_id user_id user projects_and_roles

checkEditUser :: UserId -> Handler UserId
checkEditUser user_id = do
    viewer_id <- requireAuthId
    when (user_id /= viewer_id) $ runYDB $ do
        is_admin <- isProjectAdmin "snowdrift" viewer_id
        unless is_admin $
            lift $ permissionDenied "You can only modify your own profile!"
    return viewer_id

getEditUserR :: UserId -> Handler Html
getEditUserR user_id = do
    _ <- checkEditUser user_id
    user <- runYDB (get404 user_id)

    (form, enctype) <- generateFormPost $ editUserForm (Just user)
    defaultLayout $ do
        setTitle . toHtml $ "User Profile - " <> userPrintName (Entity user_id user) <> " | Snowdrift.coop"
        $(widgetFile "edit_user")

postUserR :: UserId -> Handler Html
postUserR user_id = do
    viewer_id <- checkEditUser user_id

    ((result, _), _) <- runFormPost $ editUserForm Nothing

    case result of
        FormSuccess user_update -> do
            mode <- lookupPostParam "mode"
            let action :: Text = "update"
            case mode of
                Just "preview" -> do
                    user <- runYDB $ get404 user_id

                    let updated_user = updateUserPreview user_update user

                    (form, _) <- generateFormPost $ editUserForm (Just updated_user)

                    defaultLayout $
                        previewWidget form action $
                            renderUser (Just viewer_id) user_id updated_user mempty

                Just x | x == action -> do
                    runDB (updateUserDB user_id user_update)
                    redirect (UserR user_id)

                _ -> do
                    addAlertEm "danger" "unknown mode" "Error: "
                    redirect (UserR user_id)

        _ -> do
            alertDanger "Failed to update user."
            redirect (UserR user_id)

getUsersR :: Handler Html
getUsersR = do
    void requireAuth

    users' <- runDB $
                  select $
                  from $ \user -> do
                  orderBy [desc $ user ^. UserId]
                  return user

    infos :: [(Entity User, ((Value Text, Value Text), Value Role))] <- runDB $
        select $
        from $ \(user `InnerJoin` role `InnerJoin` project) -> do
        on_ (project ^. ProjectId ==. role ^. ProjectUserRoleProject)
        on_ (user ^. UserId ==. role ^. ProjectUserRoleUser)
        return (user, ((project ^. ProjectName, project ^. ProjectHandle), role ^. ProjectUserRoleRole))


    let users = map (\u -> (getUserKey u, u)) users'
        infos' :: [(UserId, ((Text, Text), Role))] = map (entityKey *** unwrapValues) infos
        infos'' :: [(UserId, Map (Text, Text) (Set Role))] = map (second $ uncurry M.singleton . second S.singleton) infos'
        allProjects :: Map UserId (Map (Text, Text) (Set Role)) = M.fromListWith (M.unionWith S.union) infos''
        userProjects :: Entity User -> Maybe (Map (Text, Text) (Set (Role)))
        userProjects u = M.lookup (entityKey u) allProjects
        getUserKey :: Entity User -> Text
        getUserKey (Entity key _) = either (error . T.unpack) id . fromPersistValue . unKey $ key

    defaultLayout $ do
        setTitle "Users | Snowdrift.coop"
        $(widgetFile "users")


getUserCreateR :: Handler Html
getUserCreateR = do
    (form, _) <- generateFormPost $ createUserForm Nothing
    defaultLayout $ do
        setTitle "Create User | Snowdrift.coop"
        [whamlet|
            <form method=POST>
                ^{form}
                <input type=submit>
        |]

postUserCreateR :: Handler Html
postUserCreateR = do
    ((result, form), _) <- runFormPost $ createUserForm Nothing

    case result of
        FormSuccess (ident, passwd, name, avatar, nick) -> do
            createUser ident (Just passwd) name avatar nick >>= \ maybe_user_id -> when (isJust maybe_user_id) $ do
                setCreds True $ Creds "HashDB" ident []
                redirectUltDest HomeR

        FormMissing -> alertDanger "missing field"
        FormFailure strings -> alertDanger (mconcat strings)

    defaultLayout $ [whamlet|
        <form method=POST>
            ^{form}
            <input type=submit>
    |]

-- | POST handler for marking a user as eligible for establishment.
postUserEstEligibleR :: UserId -> Handler Html
postUserEstEligibleR user_id = do
    establisher_id <- requireAuthId

    ok <- canMakeEligible user_id establisher_id
    unless ok $
        error "You can't establish this user"

    ((result, _), _) <- runFormPost establishUserForm
    case result of
        FormSuccess reason -> do
            user <- runYDB (get404 user_id)
            case userEstablished user of
                EstUnestablished -> do
                    runSDB (eligEstablishUserDB establisher_id user_id reason)
                    setMessage "This user is now eligible for establishment. Thanks!"
                    redirectUltDest HomeR
                _ -> error "User not unestablished!"
        _ -> error "Error submitting form."
