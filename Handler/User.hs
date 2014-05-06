module Handler.User where

import Import

import qualified Data.Map as Map
import Data.Universe
import qualified Data.Set as Set

import Model.User
import Model.Role


import Widgets.Markdown
import Widgets.ProjectPledges
import Widgets.Preview

import Yesod.Markdown
import Model.Markdown

import Yesod.Auth.HashDB (setPassword)

import Control.Exception.Lifted (throwIO, handle)

import qualified Data.Text as T

hiddenMarkdown :: (RenderMessage (HandlerSite m) FormMessage, MonadHandler m) => Maybe Markdown -> AForm m (Maybe Markdown)
hiddenMarkdown Nothing = fmap (fmap Markdown) $ aopt hiddenField "" Nothing
hiddenMarkdown (Just (Markdown str)) = fmap (fmap Markdown) $ aopt hiddenField "" $ Just $ Just str

editUserForm :: User -> Form UserUpdate
editUserForm user = renderBootstrap3 $
    UserUpdate
        <$> aopt' textField "Public Name" (Just $ userName user)
        <*> aopt' textField "Avatar (link)" (Just $ userAvatar user)
        <*> aopt' textField "IRC name @freenode.net)" (Just $ userIrcNick user)
        <*> aopt' snowdriftMarkdownField "Blurb (used on listings of many people)" (Just $ userBlurb user)
        <*> aopt' snowdriftMarkdownField "Personal Statement (visible only on this page)" (Just $ userStatement user)
    where


previewUserForm :: User -> Form UserUpdate
previewUserForm user = renderBootstrap3 $
    UserUpdate
        <$> aopt hiddenField "" (Just $ userName user)
        <*> aopt hiddenField "" (Just $ userAvatar user)
        <*> aopt hiddenField "" (Just $ userIrcNick user)
        <*> hiddenMarkdown (userBlurb user)
        <*> hiddenMarkdown (userStatement user)


getOldUserR :: UserId -> Handler Html
getOldUserR = redirect . UserR

getUserR :: UserId -> Handler Html
getUserR user_id = do
    maybe_viewer_id <- maybeAuthId

    {- user <- runDB $ case maybe_viewer_id of
        Nothing -> do
            on_committee <- fmap isJust $ getBy $ UniqueCommitteeMember user_id
            if on_committee
             then get404 user_id
             else permissionDenied "You must be logged in to view this user"

        Just _ -> get404 user_id
    -}

    user <- runDB $ get404 user_id
    
    project_list :: [((Value Text, Value Text), Value Role)] <- runDB $
             select $ from $ \(role `InnerJoin` project) -> do
                 on_ (role ^. ProjectUserRoleProject ==. project ^. ProjectId)
                 where_ (role ^. ProjectUserRoleUser ==. val user_id)
                 return ((project ^. ProjectName, project ^. ProjectHandle), role ^. ProjectUserRoleRole)
    
    let project_list2 = map unwrapValues project_list
    let projects = Map.fromListWith Set.union $ map (second Set.singleton) project_list2
    
    defaultLayout $ do
        setTitle . toHtml $ "User Profile - " <> userPrintName (Entity user_id user) <> " | Snowdrift.coop"
        renderUser maybe_viewer_id user_id user projects

unwrapValues ((Value a, Value b), Value c) = ((a, b), c)

renderUser :: Maybe UserId -> UserId -> User -> Map (Text, Text) (Set (Role)) -> Widget
renderUser viewer_id user_id user projects = do
    let is_owner = Just user_id == viewer_id
        user_entity = Entity user_id user
        project_handle = error "bad link - no default project on user pages" -- TODO turn this into a caught exception
        role_list = map roleLabel (universe :: [Role])
        filterRoles r = filter (\(Value r', _) -> roleLabel r' == r)

    $(widgetFile "user")


getOldEditUserR :: UserId -> Handler Html
getOldEditUserR = redirect . EditUserR

getEditUserR :: UserId -> Handler Html
getEditUserR user_id = do
    viewer_id <- requireAuthId
    when (user_id /= viewer_id) $ runDB $ do
        is_admin <- isProjectAdmin "snowdrift" viewer_id
        unless is_admin $ lift $ permissionDenied "You can only modify your own profile!"

    user <- runDB $ get404 user_id

    (form, enctype) <- generateFormPost $ editUserForm user
    defaultLayout $ do
        setTitle . toHtml $ "User Profile - " <> userPrintName (Entity user_id user) <> " | Snowdrift.coop"
        $(widgetFile "edit_user")


postOldUserR :: UserId -> Handler Html
postOldUserR = postUserR

postUserR :: UserId -> Handler Html
postUserR user_id = do
    viewer_id <- requireAuthId

    when (user_id /= viewer_id) $ runDB $ do
        is_admin <- isProjectAdmin "snowdrift" viewer_id
        unless is_admin $ lift $ permissionDenied "You can only modify your own profile!"

    ((result, _), _) <- runFormPost $ editUserForm undefined

    case result of
        FormSuccess user_update -> do
            mode <- lookupPostParam "mode"
            let action :: Text = "update"
            case mode of
                Just "preview" -> do
                    user <- runDB $ get404 user_id

                    let updated_user = applyUserUpdate user user_update

                    (form, _) <- generateFormPost $ editUserForm updated_user

                    defaultLayout $ renderPreview form action $ renderUser (Just viewer_id) user_id updated_user Map.empty

                Just x | x == action -> do
                    runDB $ updateUser user_id user_update
                    redirect $ UserR user_id

                _ -> do
                    addAlertEm "danger" "unknown mode" "Error: "
                    redirect $ UserR user_id

        _ -> do
            addAlert "danger" "Failed to update user." 
            redirect $ UserR user_id

getOldUsersR :: Handler Html
getOldUsersR = redirect UsersR

getUsersR :: Handler Html
getUsersR = do
    Entity _ viewer <- requireAuth

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
        infos'' :: [(UserId, Map (Text, Text) (Set Role))] = map (second $ uncurry Map.singleton . second Set.singleton) infos'
        allProjects :: Map UserId (Map (Text, Text) (Set Role)) = Map.fromListWith (Map.unionWith Set.union) infos''
        userProjects :: Entity User -> Maybe (Map (Text, Text) (Set (Role)))
        userProjects u = Map.lookup (entityKey u) allProjects
        getUserKey :: Entity User -> Text
        getUserKey (Entity key _) = either (error . T.unpack) id . fromPersistValue . unKey $ key

    defaultLayout $ do
        setTitle "Users | Snowdrift.coop"
        $(widgetFile "users")

getOldUserCreateR :: Handler Html
getOldUserCreateR = redirect UserCreateR

getUserCreateR :: Handler Html
getUserCreateR = do
    (form, _) <- generateFormPost $ userCreateForm Nothing
    defaultLayout $ do 
        setTitle "Create User | Snowdrift.coop"
        [whamlet|
            <form method=POST>
                ^{form}
                <input type=submit>
        |]


postOldUserCreateR :: Handler Html
postOldUserCreateR = postUserCreateR

postUserCreateR :: Handler Html
postUserCreateR = do
    ((result, form), _) <- runFormPost $ userCreateForm Nothing

    case result of
        FormSuccess (ident, passwd, name, avatar, nick) -> do
            now <- liftIO getCurrentTime
            success <- handle (\ DBException -> return False) $ runDB $ do
                account_id <- insert $ Account 0
                user <- setPassword passwd $ User ident (Just now) Nothing Nothing name account_id avatar Nothing Nothing nick now now now now Nothing Nothing
                uid_maybe <- insertUnique user
                lift $ case uid_maybe of
                    Just uid -> do
-- The addAlert here didn't render right, and anyway, the "login" alert is also showing currently and we're making a message to welcome users anyway
--                      addAlert "success" $ T.pack ("Created user; welcome! (" ++ show account_id ++ ", " ++ show uid ++ ")") 
                        return True

                    Nothing -> do
                        addAlert "danger" "E-mail or handle already in use." 
                        throwIO DBException

            when success $ do
                setCreds True $ Creds "HashDB" ident []
                redirectUltDest HomeR

        FormMissing -> addAlert "danger" "missing field" 
        FormFailure strings -> addAlert "danger" (mconcat strings) 

    defaultLayout $ [whamlet|
        <form method=POST>
            ^{form}
            <input type=submit>
    |]
    

userCreateForm :: Maybe Text -> Form (Text, Text, Maybe Text, Maybe Text, Maybe Text)
userCreateForm ident extra = do
    (identRes, identView) <- mreq textField "" ident
    (passwd1Res, passwd1View) <- mreq passwordField "" Nothing
    (passwd2Res, passwd2View) <- mreq passwordField "" Nothing
    (nameRes, nameView) <- mopt textField "" Nothing
    (avatarRes, avatarView) <- mopt textField "" Nothing
    (nickRes, nickView) <- mopt textField "" Nothing

    let view = [whamlet|
        ^{extra}
        <p>
            By registering, you agree to Snowdrift.coop's (amazingly ethical and ideal) #
                <a href="@{ToUR}">Terms of Use
                and <a href="@{PrivacyR}">Privacy Policy</a>.
        <table .table>
            <tr>
                <td>
                    E-mail or handle (private):
                <td>
                    ^{fvInput identView}
            <tr>
                <td>
                    Passphrase:
                <td>
                    ^{fvInput passwd1View}
            <tr>
                <td>
                    Repeat passphrase:
                <td>
                    ^{fvInput passwd2View}
            <tr>
                <td>
                    Name (public, optional):
                <td>
                    ^{fvInput nameView}
            <tr>
                <td>
                    Avatar (link, optional):
                <td>
                    ^{fvInput avatarView}
            <tr>
                <td>
                    IRC Nick (irc.freenode.net, optional):
                <td>
                    ^{fvInput nickView}
    |]

        passwdRes = case (passwd1Res, passwd2Res) of
            (FormSuccess a, FormSuccess b) -> if a == b then FormSuccess a else FormFailure ["passwords do not match"]
            (FormSuccess _, x) -> x
            (x, _) -> x

        result = (,,,,) <$> identRes <*> passwdRes <*> nameRes <*> avatarRes <*> nickRes

    return (result, view)
