module Handler.User where

import Import

import Data.List (head)
import Data.Maybe (fromJust)
import Data.Time.Format
import Text.Cassius (cassiusFile)
import Yesod.Auth.HashDB (setPassword, validateUser)
import qualified Data.Default as Default
import qualified Data.Map as M
import qualified Data.Maybe as Maybe
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Traversable as Traversable

import Handler.Comment as Com
import Handler.Discussion
import Handler.User.Comment
import Handler.Utils
import Model.Comment.ActionPermissions
import Model.Comment.Sql
import Model.Currency
import Model.Notification.Internal
            (UserNotificationType (..), ProjectNotificationType (..))
import Model.ResetPassword (deleteFromResetPassword)
import Model.Role
import Model.Transaction
import Model.User
import View.Comment
import View.Time
import View.User
import Widgets.Preview
import Widgets.ProjectPledges

getUsersR :: Handler Html
getUsersR = do
    void requireAuth

    (users', allProjects) <- runDB ((,) <$> fetchAllUsersDB <*> fetchAllUserProjectInfosDB)

    let users :: [(Text, Entity User)]
        users = map (\u -> (getUserKey u :: Text, u)) $ filter isVisible users'

        userProjects :: Entity User -> Maybe (Map (Text, Text) (Set (Role)))
        userProjects u = M.lookup (entityKey u) allProjects

        getUserKey :: PersistField a => Entity User -> a
        getUserKey = either (error . T.unpack) id . fromPersistValue . toPersistValue . entityKey

        isVisible :: Entity User -> Bool
        isVisible = (>= (0::Int)) . getUserKey

    defaultLayout $ do
        snowdriftTitle "Users"
        $(widgetFile "users")

--------------------------------------------------------------------------------
-- /new

getUserCreateR :: Handler Html
getUserCreateR = do
    (form, _) <- generateFormPost $ createUserForm Nothing
    defaultLayout $ do
        snowdriftTitle "Create User"
        [whamlet|
            <form method=POST>
                ^{form}
                <input type=submit>
        |]

startEmailVerification :: UserId -> Text -> HandlerT App IO ()
startEmailVerification user_id user_email = do
    hash    <- liftIO newHash
    ver_uri <- getUrlRender <*> (pure $ UserVerifyEmailR user_id hash)
    runDB $ do
        insert_ $ EmailVerification user_id user_email ver_uri False
        update $ \u -> do
            set u $ [UserEmail_verified =. val False]
            where_ $ u ^. UserId ==. val user_id
    alertSuccess $ "Verification email has been sent to " <> user_email <> "."

postUserCreateR :: Handler Html
postUserCreateR = do
    ((result, form), _) <- runFormPost $ createUserForm Nothing

    case result of
        FormSuccess (ident, passwd, name, memail, avatar, nick) -> do
            createUser ident (Just passwd) name (NewEmail False <$> memail) avatar nick
                >>= \muser_id -> when (isJust muser_id) $ do
                    when (isJust memail) $ do
                        let email   = fromJust memail
                            user_id = fromJust muser_id
                        startEmailVerification user_id email
                    setCreds True $ Creds "hashdb" ident []
                    redirectUltDest HomeR

        FormMissing -> alertDanger "missing field"
        FormFailure strings -> alertDanger (mconcat strings)

    defaultLayout $ [whamlet|
        <form method=POST>
            ^{form}
            <input type=submit>
    |]


--------------------------------------------------------------------------------
-- /#UserId

getUserR :: UserId -> Handler Html
getUserR user_id = do
    mviewer_id <- maybeAuthId

    user <- runYDB $ get404 user_id

    projects_and_roles <- runDB (fetchUserProjectsAndRolesDB user_id)
    when ( Just user_id == mviewer_id
        && isJust (userEmail user)
        && not (userEmail_verified user)
        ) $ alertWarning $ "Email address is not verified. Until you verify it, "
                    <> "you will not be able to receive email notifications."

    defaultLayout $ do
        snowdriftDashTitle "User Profile" $
            userDisplayName (Entity user_id user)
        renderUser mviewer_id user_id user projects_and_roles

postUserR :: UserId -> Handler Html
postUserR user_id = do
    void $ checkEditUser user_id
    memail <- runDB $ fetchUserEmail user_id
    case memail of
        Nothing    -> alertDanger "No email address is associated with your account."
        Just email -> startEmailVerification user_id email
    redirect $ UserR user_id

--------------------------------------------------------------------------------
-- /#UserId/balance

-- check permissions for user balance view
getUserBalanceR :: UserId -> Handler Html
getUserBalanceR user_id = do
    viewer_id <- requireAuthId
    if viewer_id /= user_id
        then permissionDenied "You must be a site administrator to view user balances."
        else getUserBalanceR' user_id

getUserBalanceR' :: UserId -> Handler Html
getUserBalanceR' user_id = do
    user <- runYDB $ get404 user_id

    -- TODO: restrict viewing balance to user or snowdrift admins (logged) before moving to real money
    -- when (user_id /= viewer_id) $ permissionDenied "You can only view your own account balance history."

    Just account <- runDB $ get $ userAccount user

    offset' <- lookupParamDefault "offset" 0
    limit' <- lookupParamDefault "count" 20

    (transactions, user_accounts, project_accounts) <- runDB $ do
        transactions <- select $ from $ \transaction -> do
            where_ ( transaction ^. TransactionCredit ==. val (Just (userAccount user))
                    ||. transaction ^. TransactionDebit ==. val (Just (userAccount user)))
            orderBy [ desc (transaction ^. TransactionTs) ]
            limit limit'
            offset offset'
            return transaction

        let accounts = catMaybes $ S.toList $ S.fromList $ map (transactionCredit . entityVal) transactions ++ map (transactionDebit . entityVal) transactions

        users <- selectList [ UserAccount <-. accounts ] []
        projects <- selectList [ ProjectAccount <-. accounts ] []

        let mkMapBy :: Ord b => (a -> b) -> [a] -> M.Map b a
            mkMapBy f = M.fromList . map (\e -> (f e, e))

        return
            ( transactions
            , mkMapBy (userAccount . entityVal) users
            , mkMapBy (projectAccount . entityVal) projects
            )

    (add_funds_form, _) <- generateFormPost addTestCashForm

    defaultLayout $ do
        snowdriftDashTitle "User Balance" $
            userDisplayName (Entity user_id user)
        $(widgetFile "user_balance")
  where
    -- Warning: We can do better than 'read'.
    lookupParamDefault :: Text -> Int64 -> Handler Int64
    lookupParamDefault name def = do
        maybe_param <- lookup name <$> reqGetParams <$> getRequest
        return $ fromMaybe def $ do
            param_str <- maybe_param
            param <- listToMaybe $ reads $ T.unpack param_str
            return $ fst param

postUserBalanceR :: UserId -> Handler Html
postUserBalanceR user_id = do
    Entity viewer_id _ <- requireAuth
    user <- runYDB $ get404 user_id
    unless (user_id == viewer_id) $
        permissionDenied "You can only add money to your own account."

    ((result, _), _) <- runFormPost addTestCashForm

    now <- liftIO getCurrentTime

    let balanceCap :: Milray
        balanceCap = 1000000

    case result of
        FormSuccess amount -> do
            if amount < 10
                then alertDanger "Sorry, minimum deposit is $10"
                else do
                    success <- runDB $ do
                        c <- updateCount $ \account -> do
                            set account [ AccountBalance +=. val amount ]
                            where_ $ account ^. AccountId ==. val (userAccount user)
                                &&. account ^. AccountBalance +. val amount <=. val balanceCap

                        when (c == 1) $ insert_ $ Transaction now (Just $ userAccount user) Nothing Nothing amount "Test Load" Nothing
                        return (c == 1)

                    if success
                     then alertSuccess "Balance updated."
                     else alertDanger $ "Balance would exceed (testing only) cap of " <> T.pack (show balanceCap)

            redirect (UserBalanceR user_id)

        _ -> error "Error processing form."


--------------------------------------------------------------------------------
-- /#UserId/d

-- | getUserDiscussionR generates the associated discussion page for each user
getUserDiscussionR :: UserId -> Handler Html
getUserDiscussionR user_id = getDiscussion (getUserDiscussionR' user_id)

getUserDiscussionR'
        :: UserId
        -> (DiscussionId -> ExprCommentCond -> DB [Entity Comment])  -- ^ Root comment getter.
        -> Handler Html
getUserDiscussionR' user_id get_root_comments = do
    mviewer <- maybeAuth
    let mviewer_id = entityKey <$> mviewer

    (user, root_comments) <- runYDB $ do
        user <- get404 user_id
        let has_permission = (exprCommentUserPermissionFilter mviewer_id (val user_id))
        root_comments <- get_root_comments (userDiscussion user) has_permission
        return (user, root_comments)

    (comment_forest_no_css, _) <-
        makeUserCommentForestWidget
            mviewer
            user_id
            root_comments
            Default.def
            getMaxDepth
            False
            mempty

    let has_comments = not (null root_comments)
        comment_forest = do
            comment_forest_no_css
            toWidget $(cassiusFile "templates/comment.cassius")

    (comment_form, _) <- generateFormPost commentNewTopicForm

    defaultLayout $ do
        snowdriftTitle $
            userDisplayName (Entity user_id user) <>
            " User Discussion"
        $(widgetFile "user_discuss")

--------------------------------------------------------------------------------
-- /#target/d/new

getNewUserDiscussionR :: UserId -> Handler Html
getNewUserDiscussionR user_id = do
    void requireAuth
    let widget = commentNewTopicFormWidget
    defaultLayout $(widgetFile "user_discussion_wrapper")

postNewUserDiscussionR :: UserId -> Handler Html
postNewUserDiscussionR user_id = do
    viewer <- requireAuth
    User{..} <- runYDB $ get404 user_id

    postNewComment
      Nothing
      viewer
      userDiscussion
      (makeUserCommentActionPermissionsMap (Just viewer) user_id Default.def) >>= \case
           ConfirmedPost (Left err) -> do
               alertDanger err
               redirect $ NewUserDiscussionR user_id
           ConfirmedPost (Right comment_id) ->
               redirect $ UserCommentR user_id comment_id
           Com.Preview (widget, form) ->
               defaultLayout $ previewWidget form "post" $
                   userDiscussionPage user_id widget

postUserDiscussionR :: UserId -> Handler Html
postUserDiscussionR _ = error "TODO(mitchell)"

--------------------------------------------------------------------------------
-- /#UserId/change-password

getUserChangePasswordR :: UserId -> Handler Html
getUserChangePasswordR user_id = do
    void $ checkEditUser user_id
    user <- runYDB $ get404 user_id
    (form, enctype) <- generateFormPost changePasswordForm
    defaultLayout $ do
        snowdriftDashTitle "Change Passphrase" $
            userDisplayName (Entity user_id user)
        $(widgetFile "change_password")

resetPassword :: RedirectUrl App route
              => UserId -> User -> Text -> Text -> route -> Handler Html
resetPassword user_id user password password' route =
    if password == password'
        then do
            user' <- setPassword password user
            runDB $ do
                updateUserPasswordDB user_id (userHash user') (userSalt user')
                deleteFromResetPassword user_id
            alertSuccess "You successfully updated your passphrase."
            redirect $ UserR user_id
        else do
            alertDanger "The passphrases you entered do not match."
            redirect route

postUserChangePasswordR :: UserId -> Handler Html
postUserChangePasswordR user_id = do
    void $ checkEditUser user_id
    ((result, form), enctype) <- runFormPost changePasswordForm
    case result of
        FormSuccess ChangePassword {..} -> do
            user <- runYDB $ get404 user_id
            is_valid_password <- validateUser (UniqueUser $ userIdent user)
                                     currentPassword
            if is_valid_password
                then resetPassword user_id user newPassword newPassword' $
                         UserChangePasswordR user_id
                else do
                    alertDanger "Sorry, that is not the correct current passphrase."
                    defaultLayout $(widgetFile "change_password")
        _ -> do
            alertDanger "Oops, failed to update the passphrase."
            defaultLayout $(widgetFile "change_password")

--------------------------------------------------------------------------------
-- /#UserId/delete

startDeleteConfirmation :: UserId -> Handler ()
startDeleteConfirmation user_id = do
    hash        <- liftIO newHash
    confirm_uri <- getUrlRender <*> (pure $ UserConfirmDeleteR user_id hash)
    muser_email <- runDB $ fetchUserEmailVerified user_id
    case muser_email of
        Nothing -> alertDanger $
            "Cannot continue without a verified email address. " <>
            "Please add one to your profile and verify it."
        Just user_email -> do
            runDB $ insert_ $
                DeleteConfirmation user_id user_email confirm_uri False
            alertSuccess $
                "Confirmation email has been sent to " <> user_email <> "."

getDeleteUserR :: UserId -> Handler Html
getDeleteUserR user_id = do
    void $ checkEditUser user_id
    user <- runYDB $ get404 user_id
    defaultLayout $ do
        snowdriftDashTitle "Delete Account" $
            userDisplayName (Entity user_id user)
        $(widgetFile "delete_user")

postDeleteUserR :: UserId -> Handler Html
postDeleteUserR user_id = do
    void $ checkEditUser user_id
    startDeleteConfirmation user_id
    redirect $ UserR user_id

--------------------------------------------------------------------------------
-- /#UserId/confirm-delete/#Text

checkConfirmDelete :: UserId -> Text -> Handler User
checkConfirmDelete user_id hash = do
    confirm_uri <- getUrlRender <*> (pure $ UserConfirmDeleteR user_id hash)
    muser_email <- runDB $ fetchUserEmail user_id
    case muser_email of
        Nothing    -> notFound
        Just email -> runYDB $ do
            -- Check whether the hash is in the DB.
            void $ getBy404 $ UniqueDeleteConfirmation user_id email confirm_uri
            get404 user_id

getUserConfirmDeleteR :: UserId -> Text -> Handler Html
getUserConfirmDeleteR user_id hash = do
    void $ checkEditUser user_id
    user <- checkConfirmDelete user_id hash
    defaultLayout $ do
        snowdriftDashTitle "Delete Account" $
            userDisplayName (Entity user_id user)
        $(widgetFile "user_confirm_delete")

postUserConfirmDeleteR :: UserId -> Text -> Handler Html
postUserConfirmDeleteR user_id hash = do
    void $ checkEditUser user_id
    void $ checkConfirmDelete user_id hash
    runDB $ deleteUserDB user_id
    alertSuccess "Successfully deleted your account."
    redirect HomeR

--------------------------------------------------------------------------------
-- /#UserId/edit

getEditUserR :: UserId -> Handler Html
getEditUserR user_id = do
    _ <- checkEditUser user_id
    user <- runYDB (get404 user_id)

    (form, enctype) <- generateFormPost $ editUserForm (Just user)
    defaultLayout $ do
        snowdriftDashTitle "User Profile" $
            userDisplayName (Entity user_id user)
        $(widgetFile "edit_user")

postEditUserR :: UserId -> Handler Html
postEditUserR user_id = do
    viewer_id <- checkEditUser user_id

    ((result, _), _) <- runFormPost $ editUserForm Nothing

    case result of
        FormSuccess user_update -> do
            lookupPostMode >>= \case
                Just PostMode -> do
                    let muser_email = userUpdateEmail user_update
                    when (isJust muser_email) $ do
                        let user_email = fromJust muser_email
                        mcurrent_email <- runDB $ fetchUserEmail user_id
                        when (mcurrent_email /= Just user_email) $
                            startEmailVerification user_id user_email
                    runDB (updateUserDB user_id user_update)
                    redirect (UserR user_id)

                _ -> do
                    user <- runYDB $ get404 user_id

                    let updated_user = updateUserPreview user_update user

                    (form, _) <- generateFormPost $ editUserForm (Just updated_user)

                    defaultLayout $
                        previewWidget form "update" $
                            renderUser (Just viewer_id) user_id updated_user mempty
        _ -> do
            alertDanger "Failed to update user."
            redirect (UserR user_id)

checkEditUser :: UserId -> Handler UserId
checkEditUser user_id = do
    viewer_id <- requireAuthId
    unless (user_id == viewer_id) $
        permissionDenied "You can only modify your own profile."
    return viewer_id

--------------------------------------------------------------------------------
-- /#UserId/elig

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
                    honor_pledge <- getUrlRender >>= \r -> return $ r HonorPledgeR
                    runSDB $ eligEstablishUserDB honor_pledge establisher_id user_id reason
                    setMessage "This user is now eligible for establishment. Thanks!"
                    redirectUltDest HomeR
                _ -> error "User not unestablished!"
        _ -> error "Error submitting form."

--------------------------------------------------------------------------------
-- /#UserId/verify-email/#Text

getUserVerifyEmailR :: UserId -> Text -> Handler Html
getUserVerifyEmailR user_id hash = do
    void $ checkEditUser user_id
    ver_uri <- getUrlRender <*> (pure $ UserVerifyEmailR user_id hash)
    (mver_email, muser_email) <- runDB $ (,)
        <$> fetchVerEmail ver_uri user_id
        <*> fetchUserEmail user_id
    if | Maybe.isNothing mver_email -> notFound
       | Maybe.isNothing muser_email -> do
             alertDanger $ "Failed to verify the email address since none is "
                        <> "associated with the account."
             redirect HomeR
       | otherwise -> do
             let ver_email  = fromJust mver_email
                 user_email = fromJust muser_email
             if ver_email == user_email
                 then do
                     runDB $ verifyEmailDB user_id
                     alertSuccess "Successfully verified the email address."
                     redirect HomeR
                 else do
                     alertDanger $ "Current email address does not match the "
                                <> "verification link."
                     redirect HomeR

--------------------------------------------------------------------------------
-- /#UserId/pledges

getUserPledgesR :: UserId -> Handler Html
getUserPledgesR user_id = do
    -- TODO: refine permissions here
    _ <- requireAuthId
    user <- runYDB $ get404 user_id
    defaultLayout $ do
        snowdriftDashTitle "User Pledges" $
            userDisplayName (Entity user_id user)

        $(widgetFile "user_pledges")

--------------------------------------------------------------------------------
-- /#UserId/t

getUserTicketsR :: UserId -> Handler Html
getUserTicketsR user_id = do
    user <- runYDB $ get404 user_id
    mviewer_id <- maybeAuthId
    claimed_tickets <- claimedTickets user_id
    watched_tickets <- watchedTickets user_id

    defaultLayout $ do
        snowdriftDashTitle "User Tickets" $
            userDisplayName (Entity user_id user)

        $(widgetFile "user_tickets")

--------------------------------------------------------------------------------
-- /#UserId/user-notifications

getUserNotificationsR :: UserId -> Handler Html
getUserNotificationsR user_id = do
    void $ checkEditUser user_id
    user <- runYDB $ get404 user_id
    let fetchNotifPref = runYDB . fetchUserNotificationPrefDB user_id
    mbal   <- fetchNotifPref NotifBalanceLow
    mucom  <- fetchNotifPref NotifUnapprovedComment
    mrcom  <- fetchNotifPref NotifRethreadedComment
    mrep   <- fetchNotifPref NotifReply
    mecon  <- fetchNotifPref NotifEditConflict
    mflag  <- fetchNotifPref NotifFlag
    mflagr <- fetchNotifPref NotifFlagRepost
    is_moderator    <- runDB $ userIsModerator user_id
    (form, enctype) <- generateFormPost $
        userNotificationsForm is_moderator
            mbal mucom mrcom mrep mecon mflag mflagr
    defaultLayout $ do
        snowdriftDashTitle "Notification Preferences" $
            userDisplayName (Entity user_id user)
        $(widgetFile "user_notifications")

postUserNotificationsR :: UserId -> Handler Html
postUserNotificationsR user_id = do
    void $ checkEditUser user_id
    is_moderator <- runDB $ userIsModerator user_id
    ((result, form), enctype) <- runFormPost $
        userNotificationsForm is_moderator
            Nothing Nothing Nothing Nothing Nothing Nothing Nothing
    case result of
        FormSuccess notif_pref -> do
            forM_ (userNotificationPref notif_pref) $ \(ntype, ndeliv) ->
                runDB $ updateUserNotificationPrefDB user_id ntype ndeliv
            alertSuccess "Successfully updated the notification preferences."
            redirect $ UserR user_id
        _ -> do
            alertDanger $ "Failed to update the notification preferences. "
                       <> "Please try again."
            defaultLayout $(widgetFile "user_notifications")

--------------------------------------------------------------------------------
-- /#UserId/select-project

getUserSelectProjectR :: UserId -> Handler Html
getUserSelectProjectR user_id = do
    void $ checkEditUser user_id
    user <- runYDB $ get404 user_id
    projects <- runDB $ fetchUserWatchingProjectsDB user_id
    if length projects == 1
        then redirect $ ProjectNotificationsR user_id $ entityKey $ head projects
        else defaultLayout $ do
            snowdriftDashTitle "Select Project" $
                userDisplayName (Entity user_id user)
            $(widgetFile "user_select_project")

postUserSelectProjectR :: UserId -> Handler Html
postUserSelectProjectR user_id = do
    void $ checkEditUser user_id
    mproject_id <- lookupPostParam "project_id"
    maybe (redirect $ UserR user_id)
          (redirect . ProjectNotificationsR user_id . key . PersistInt64)
          (join $ Traversable.forM mproject_id $ readMaybe . T.unpack)

--------------------------------------------------------------------------------
-- /#UserId/project-notifications

getProjectNotificationsR :: UserId -> ProjectId -> Handler Html
getProjectNotificationsR user_id project_id = do
    void $ checkEditUser user_id
    user    <- runYDB $ get404 user_id
    project <- runYDB $ get404 project_id
    let fetchNotifPref =
            runYDB . fetchProjectNotificationPrefDB user_id project_id
    mwiki_page        <- fetchNotifPref NotifWikiPage
    mwiki_edit        <- fetchNotifPref NotifWikiEdit
    mblog_post        <- fetchNotifPref NotifBlogPost
    mnew_pledge       <- fetchNotifPref NotifNewPledge
    mupdated_pledge   <- fetchNotifPref NotifUpdatedPledge
    mdeleted_pledge   <- fetchNotifPref NotifDeletedPledge
    (form, enctype) <- generateFormPost $
        projectNotificationsForm mwiki_page mwiki_edit mblog_post
                                 mnew_pledge mupdated_pledge mdeleted_pledge
    defaultLayout $ do
        snowdriftDashTitle
            ("Notification Preferences for " <> projectName project)
            (userDisplayName $ Entity user_id user)
        $(widgetFile "project_notifications")

postProjectNotificationsR :: UserId -> ProjectId -> Handler Html
postProjectNotificationsR user_id project_id = do
    void $ checkEditUser user_id
    ((result, form), enctype) <- runFormPost $
        projectNotificationsForm Nothing Nothing Nothing
                                 Nothing Nothing Nothing
    case result of
        FormSuccess notif_pref -> do
            forM_ (projectNotificationPref notif_pref) $ \(ntype, ndeliv) ->
                runDB $ updateProjectNotificationPrefDB
                    user_id project_id ntype ndeliv
            alertSuccess "Successfully updated the notification preferences."
            redirect (UserR user_id)
        _ -> do
            project <- runYDB $ get404 project_id
            alertDanger $ "Failed to update the notification preferences."
            defaultLayout $(widgetFile "project_notifications")

--------------------------------------------------------------------------------
-- /#UserId/reset-password/#Text

checkResetPassword :: UserId -> Text -> Handler User
checkResetPassword user_id hash = do
    uri    <- getUrlRender <*> (pure $ UserResetPasswordR user_id hash)
    memail <- runDB $ fetchUserEmail user_id
    case memail of
        Nothing    -> notFound
        Just email -> do
            runYDB $ do
                -- Check whether the hash is in the DB.
                void $ getBy404 $ UniquePasswordReset user_id email uri
                get404 user_id

getUserResetPasswordR :: UserId -> Text -> Handler Html
getUserResetPasswordR user_id hash = do
    user <- checkResetPassword user_id hash
    (form, enctype) <- generateFormPost setPasswordForm
    defaultLayout $ do
        snowdriftDashTitle "Set Passphrase" $
            userDisplayName (Entity user_id user)
        $(widgetFile "set_password")

postUserResetPasswordR :: UserId -> Text -> Handler Html
postUserResetPasswordR user_id hash = do
    user <- checkResetPassword user_id hash
    ((result, form), enctype) <- runFormPost setPasswordForm
    case result of
        FormSuccess SetPassword {..} -> do
            resetPassword user_id user password password' $
                UserResetPasswordR user_id hash
        _ -> do
            alertDanger "Oops, failed to set the passphrase."
            defaultLayout $(widgetFile "set_password")
