module View.User
    ( addTestCashForm
    , createUserForm
    , changePassphraseForm
    , editUserForm
    , establishUserForm
    , previewUserForm
    , projectNotificationsForm
    , renderUser
    , setPassphraseForm
    , userNameWidget
    , userNotificationsForm
    ) where

import Import hiding (UserNotificationPref, ProjectNotificationPref)

import Data.String (fromString)
import qualified Data.Map as M
import qualified Data.Set as S

import Avatar
import DeprecatedBootstrap
import Model.Currency
import Model.Markdown
import Model.Role
import Model.User
import Model.User.Internal
import Widgets.Markdown (snowdriftMarkdownField)
import Widgets.UserPledges

createUserForm :: Maybe Text
               -> Form (Text
                       ,Text
                       ,Maybe Text
                       ,Maybe Text
                       ,Maybe Text
                       ,Maybe Text
                       )
createUserForm ident extra = do
    (identRes,   identView)   <- mreq textField     "" ident
    -- we use "passphrase" usually, but passwordField is Yesod term
    (passph1Res, passph1View) <- mreq passwordField "" Nothing
    (passph2Res, passph2View) <- mreq passwordField "" Nothing
    (nameRes,    nameView)    <- mopt textField     "" Nothing
    (emailRes,   emailView)   <- mopt emailField    "" Nothing
    (avatarRes,  avatarView)  <- mopt textField     "" Nothing
    (nickRes,    nickView)    <- mopt textField     "" Nothing

    let view = $(widgetFile "auth/create-account-form")
        passphRes = case (passph1Res, passph2Res) of
            (FormSuccess a, FormSuccess b)
                | a == b    -> FormSuccess a
                | otherwise -> FormFailure ["passphrases do not match"]
            (FormSuccess _, x) -> x
            (x, _) -> x

        result = (,,,,,) <$> identRes <*> passphRes <*> nameRes
                         <*> emailRes <*> avatarRes <*> nickRes

    return (result, view)

editUserForm :: Maybe User -> Form UserUpdate
editUserForm muser = renderBootstrap3 BootstrapBasicForm $
    UserUpdate
        <$> aopt' textField "Public Name" (userName <$> muser)
        <*> aopt' textField "Avatar image (link)" (userAvatar <$> muser)
        <*> aopt' emailField "Email (not shown publicly)" (userEmail <$> muser)
        <*> aopt' textField "IRC nick @freenode.net" (userIrcNick <$> muser)
        <*> aopt snowdriftMarkdownField
                 (FieldSettings "Blurb (short note shown in various listings)"
                                Nothing
                                Nothing
                                Nothing
                                [("class", "form-control")
                                ,("cols", "60")
                                ,("rows", "3")])
                 (userBlurb <$> muser)
        <*> aopt snowdriftMarkdownField
                 (FieldSettings "Statement (longer info or thoughts shown only at profile)"
                                Nothing
                                Nothing
                                Nothing
                                [("class", "form-control")
                                ,("cols", "80")
                                ,("rows", "15")])
                 (userStatement <$> muser)

-- we use "passphrase" usually in our code, but passwordField is Yesod term
changePassphraseForm :: Form ChangePassphrase
changePassphraseForm = renderBootstrap3 BootstrapBasicForm $ ChangePassphrase
    <$> areq' passwordField "Current passphrase" Nothing
    <*> areq' passwordField "New passphrase" Nothing
    <*> areq' passwordField "Repeat" Nothing

-- | Form to mark a user as eligible for establishment. The user is fully established
-- when s/he accepts the honor pledge.
establishUserForm :: Form Text
establishUserForm =
    renderBootstrap3
        BootstrapBasicForm
        (areq' textField "Reason" Nothing)

previewUserForm :: User -> Form UserUpdate
previewUserForm User{..} = renderBootstrap3 BootstrapBasicForm $
    UserUpdate
        <$> aopt hiddenField "" (Just userName)
        <*> aopt hiddenField "" (Just userAvatar)
        <*> aopt hiddenField "" (Just userEmail)
        <*> aopt hiddenField "" (Just userIrcNick)
        <*> hiddenMarkdown userBlurb
        <*> hiddenMarkdown userStatement

-- | Render a User profile
renderUser :: Maybe UserId
           -> UserId
           -> User
           -> Map (Entity Project) (Set Role)
           -> Widget
renderUser mviewer_id user_id user projects_and_roles = do
    let user_entity = Entity user_id user

    should_show_est_form <- handlerToWidget (canCurUserMakeEligible user_id)
    mest_form_and_enctype <-
        if should_show_est_form
            then Just <$> handlerToWidget (generateFormPost establishUserForm)
            else return Nothing

    avatar <- getUserAvatar (StaticR img_default_avatar_png) (Just user)

    $(widgetFile "user")

setPassphraseForm :: Form SetPassphrase
setPassphraseForm = renderBootstrap3 BootstrapBasicForm $ SetPassphrase
    <$> areq' passwordField "New passphrase" Nothing
    <*> areq' passwordField "Repeat"         Nothing

hiddenMarkdown :: Maybe Markdown -> AForm Handler (Maybe Markdown)
hiddenMarkdown Nothing               = fmap (fmap Markdown) $ aopt hiddenField "" Nothing
hiddenMarkdown (Just (Markdown str)) = fmap (fmap Markdown) $ aopt hiddenField "" (Just $ Just str)


-- | Fetch user by UserId and display with userDisplayName
-- (try not to use where it means O(N) fetches of users)
userNameWidget :: UserId -> Widget
userNameWidget user_id = do
    maybe_user <- handlerToWidget $ runDB $ get user_id
    case maybe_user of
        Nothing -> [whamlet|deleted user|]
        Just user ->
            [whamlet|
                <a href=@{UserR user_id}>
                    #{userDisplayName (Entity user_id user)}
            |]

addTestCashForm :: Form Milray
addTestCashForm = renderBootstrap3 BootstrapBasicForm $ fromInteger . (10000 *) <$> areq' intField "Add (fake) money to your account (in whole dollars)" (Just 10)

req :: Eq a => [(Text, a)] -> SomeMessage App -> Maybe a
    -> AForm (HandlerT App IO) a
req methods s xs = areq' (dropdown methods) s xs

opt :: Eq a => [(Text, a)] -> SomeMessage App -> Maybe a
    -> AForm (HandlerT App IO) (Maybe a)
opt methods s xs = aopt' (dropdown methods) s (Just xs)

dropdown :: Eq a => [(Text, a)] -> Field (HandlerT App IO) a
dropdown methods = selectFieldList methods

userMethods :: [(Text, UserNotificationDelivery)]
userMethods =
    -- XXX: Support 'NotifDeliverEmailDigest'.
    [ ("website",           UserNotifDeliverWebsite)
    , ("email",             UserNotifDeliverEmail)
    , ("website and email", UserNotifDeliverWebsiteAndEmail)
    ]

projectMethods :: [(Text, ProjectNotificationDelivery)]
projectMethods =
    -- XXX: Support 'NotifDeliverEmailDigest'.
    [ ("website",           ProjectNotifDeliverWebsite)
    , ("email",             ProjectNotifDeliverEmail)
    , ("website and email", ProjectNotifDeliverWebsiteAndEmail)
    ]

userNotificationsForm :: Bool
                      -> Maybe UserNotificationDelivery
                      -> Maybe UserNotificationDelivery
                      -> Maybe UserNotificationDelivery
                      -> Maybe UserNotificationDelivery
                      -> Maybe UserNotificationDelivery
                      -> Maybe UserNotificationDelivery
                      -> Maybe UserNotificationDelivery
                      -> Form UserNotificationPref
userNotificationsForm is_moderator mbal mucom mrcom mrep mecon mflag mflagr =
    renderBootstrap3 BootstrapBasicForm $ UserNotificationPref
        <$> userReq (fromString $ "You have a low balance (less than 3 months " <>
                    "funds at current pledge levels)")    mbal
        <*> unapproved_comment
        <*> userOpt "Your comment gets rethreaded/moved"  mrcom
        <*> userOpt "Reply posted to your comment"        mrep
        <*> userReq "Your wiki post has an edit conflict" mecon
        <*> userReq "Your comment gets flagged"           mflag
        <*> userOpt "A comment you flagged gets reposted" mflagr
  where
    userReq = req userMethods
    userOpt = opt userMethods
    unapproved_comment =
        if is_moderator
            then Just <$> userReq "A new comment awaits moderator approval" mucom
            else pure Nothing

projectNotificationsForm
    :: Bool -- Is the viewer a project team member?
    -> Maybe ProjectNotificationDelivery
    -> Maybe ProjectNotificationDelivery
    -> Maybe ProjectNotificationDelivery
    -> Maybe ProjectNotificationDelivery
    -> Maybe ProjectNotificationDelivery
    -> Maybe ProjectNotificationDelivery
    -> Maybe ProjectNotificationDelivery
    -> Form ProjectNotificationPref
projectNotificationsForm is_team_member mwiki_page mwiki_edit mblog_post
                         mnew_pledge mupdated_pledge mdeleted_pledge
                         mvolunteer_app =
    renderBootstrap3 BootstrapBasicForm $ ProjectNotificationPref
        <$> projectOpt "Wiki page created" mwiki_page
        <*> projectOpt "Wiki page edited"  mwiki_edit
        <*> projectOpt "New blog post"     mblog_post
        <*> projectOpt "New pledge"        mnew_pledge
        <*> projectOpt "Pledge updated"    mupdated_pledge
        <*> projectOpt "Pledge deleted"    mdeleted_pledge
        <*> if is_team_member
                then projectOpt "Volunteer application submitted" mvolunteer_app
                else pure Nothing
  where
    projectOpt = opt projectMethods
