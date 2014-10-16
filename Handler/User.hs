module Handler.User where

import Import

import           Handler.Utils
import           Handler.Comment
import           Handler.Discussion
import           Handler.User.Comment
import           Model.Comment.ActionPermissions
import           Model.Role
import           Model.Transaction
import           Model.User
import           Model.Comment.Sql
import           Widgets.Preview
import           View.Comment
import           View.User
import           Widgets.ProjectPledges
import           Widgets.Time

import qualified Data.Map  as M
import qualified Data.Set  as S
import qualified Data.Text as T

import           Data.Default         (def)
import           Text.Cassius         (cassiusFile)

--------------------------------------------------------------------------------
-- Utility functions

lookupParamDefault :: Read a => Text -> a -> Handler a
lookupParamDefault name def = do
    maybe_param <- lookup name <$> reqGetParams <$> getRequest
    return $ fromMaybe def $ do
        param_str <- maybe_param
        param <- listToMaybe $ reads $ T.unpack param_str
        return $ fst param

--------------------------------------------------------------------------------
-- /

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

--------------------------------------------------------------------------------
-- /new

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


--------------------------------------------------------------------------------
-- /#UserId

getUserR :: UserId -> Handler Html
getUserR user_id = do
    mviewer_id <- maybeAuthId

    user <- runYDB $ get404 user_id

    projects_and_roles <- runDB (fetchUserProjectsAndRolesDB user_id)

    defaultLayout $ do
        setTitle . toHtml $ "User Profile - " <> userDisplayName (Entity user_id user) <> " | Snowdrift.coop"
        renderUser mviewer_id user_id user projects_and_roles

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
        transactions <- select $ from $ \ transaction -> do
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
            mkMapBy f = M.fromList . map (\ e -> (f e, e))

        return
            ( transactions
            , mkMapBy (userAccount . entityVal) users
            , mkMapBy (projectAccount . entityVal) projects
            )

    (add_funds_form, _) <- generateFormPost addTestCashForm

    defaultLayout $ do
        setTitle . toHtml $ "User Balance - " <> userDisplayName (Entity user_id user) <> " | Snowdrift.coop"
        $(widgetFile "user_balance")

postUserBalanceR :: UserId -> Handler Html
postUserBalanceR user_id = do
    Entity viewer_id _ <- requireAuth
    user <- runYDB $ get404 user_id
    unless (user_id == viewer_id) $
        permissionDenied "You can only add money to your own account."

    ((result, _), _) <- runFormPost addTestCashForm

    now <- liftIO getCurrentTime

    case result of
        FormSuccess amount -> do
            if amount < 10
                then alertDanger "Sorry, minimum deposit is $10"
                else do
                    runDB $ do
                        _ <- insert $ Transaction now (Just $ userAccount user) Nothing Nothing amount "Test Load" Nothing
                        update $ \ account -> do
                            set account [ AccountBalance +=. val amount ]
                            where_ ( account ^. AccountId ==. val (userAccount user) )

                    alertSuccess "Balance updated."
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
            def
            getMaxDepth
            False
            mempty

    let has_comments = not (null root_comments)
        comment_forest = do
            comment_forest_no_css
            toWidget $(cassiusFile "templates/comment.cassius")

    (comment_form, _) <- generateFormPost commentNewTopicForm

    defaultLayout $ do
        setTitle . toHtml $ userDisplayName (Entity user_id user) <> " User Discussion | Snowdrift.coop"
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
      (makeUserCommentActionPermissionsMap (Just viewer) user_id def) >>= \case
        Left comment_id -> redirect (UserCommentR user_id comment_id)
        Right (widget, form) -> defaultLayout $ previewWidget form "post" (userDiscussionPage user_id widget)

postUserDiscussionR :: UserId -> Handler Html
postUserDiscussionR _ = error "TODO(mitchell)"

--------------------------------------------------------------------------------
-- /#UserId/edit

getEditUserR :: UserId -> Handler Html
getEditUserR user_id = do
    _ <- checkEditUser user_id
    user <- runYDB (get404 user_id)

    (form, enctype) <- generateFormPost $ editUserForm (Just user)
    defaultLayout $ do
        setTitle . toHtml $ "User Profile - " <> userDisplayName (Entity user_id user) <> " | Snowdrift.coop"
        $(widgetFile "edit_user")

postUserR :: UserId -> Handler Html
postUserR user_id = do
    viewer_id <- checkEditUser user_id

    ((result, _), _) <- runFormPost $ editUserForm Nothing

    case result of
        FormSuccess user_update -> do
            lookupPostMode >>= \case
                Just PostMode -> do
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
                    runSDB (eligEstablishUserDB establisher_id user_id reason)
                    setMessage "This user is now eligible for establishment. Thanks!"
                    redirectUltDest HomeR
                _ -> error "User not unestablished!"
        _ -> error "Error submitting form."

--------------------------------------------------------------------------------
-- /#UserId/pledges

getUserPledgesR :: UserId -> Handler Html
getUserPledgesR user_id = do
    -- TODO: refine permissions here
    _ <- requireAuthId
    user <- runYDB $ get404 user_id
    defaultLayout $ do
        setTitle . toHtml $
            "User Pledges - " <> userDisplayName (Entity user_id user) <> " | Snowdrift.coop"

        $(widgetFile "user_pledges")

--------------------------------------------------------------------------------
-- /#UserId/t

getUserTicketsR :: UserId -> Handler Html
getUserTicketsR user_id = do
    user <- runYDB $ get404 user_id

    -- TODO: abstract out grabbing the project
    tickets <- runDB $ select $ from $ \ (c `InnerJoin` t `InnerJoin` tc `LeftOuterJoin` w `InnerJoin` p) -> do
        on_ $ p ^. ProjectDiscussion ==. c ^. CommentDiscussion ||. w ?. WikiPageProject ==. just (p ^. ProjectId)
        on_ $ w ?. WikiPageDiscussion ==. just (c ^. CommentDiscussion)
        on_ $ tc ^. TicketClaimingTicket ==. c ^. CommentId
        on_ $ t ^. TicketComment ==. c ^. CommentId

        where_ $ tc ^. TicketClaimingUser ==. val user_id
            &&. isNothing (tc ^. TicketClaimingReleasedTs)
            &&. c ^. CommentId `notIn` (subList_select $ from $ return . (^. CommentClosingComment))

        orderBy [ asc $ tc ^. TicketClaimingTs ]

        return (t, w, p ^. ProjectHandle)

    defaultLayout $ do
        setTitle . toHtml $
            "User Tickets - " <> userDisplayName (Entity user_id user) <> " | Snowdrift.coop"

        $(widgetFile "user_tickets")

