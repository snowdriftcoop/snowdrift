module View.User
    ( addTestCashForm
    , createUserForm
    , changePasswordForm
    , editUserForm
    , establishUserForm
    , previewUserForm
    , renderUser
    , setPasswordForm
    , userNameWidget
    , userNotificationsForm
    ) where

import Import

import           Model.Currency
import           Model.Markdown
import           Model.Notification     (NotificationDelivery (..))
import           Model.Role
import           Model.User
import           Model.User.Internal
import           Widgets.Markdown       (snowdriftMarkdownField)
import           Widgets.ProjectPledges

import           Data.List.NonEmpty     (NonEmpty)
import qualified Data.List.NonEmpty     as N
import qualified Data.Map               as M
import qualified Data.Set               as S
import           Yesod.Markdown

createUserForm :: Maybe Text -> Form (Text, Text, Maybe Text, Maybe Text, Maybe Text, Maybe Text)
createUserForm ident extra = do
    (identRes,   identView)   <- mreq textField     "" ident
    (passwd1Res, passwd1View) <- mreq passwordField "" Nothing
    (passwd2Res, passwd2View) <- mreq passwordField "" Nothing
    (nameRes,    nameView)    <- mopt textField     "" Nothing
    (emailRes,   emailView)   <- mopt emailField    "" Nothing
    (avatarRes,  avatarView)  <- mopt textField     "" Nothing
    (nickRes,    nickView)    <- mopt textField     "" Nothing

    let view = [whamlet|
        ^{extra}
        <p>
            By registering, you agree to Snowdrift.coop's (amazingly ethical and ideal) #
                <a href="@{ToUR}">Terms of Use
                and <a href="@{PrivacyR}">Privacy Policy</a>.

        <table .table>
            <tr>
                <td>
                    <label for=#{fvId identView}>
                        Handle (private):
                <td>
                    ^{fvInput identView}
            <tr>
                <td>
                    <label for=#{fvId passwd1View}>
                        Passphrase:
                <td>
                    ^{fvInput passwd1View}
            <tr>
                <td>
                    <label for=#{fvId passwd2View}>
                        Repeat passphrase:
                <td>
                    ^{fvInput passwd2View}
            <tr>
                <td>
                    <label for=#{fvId nameView}>
                        Name (public, optional):
                <td>
                    ^{fvInput nameView}
            <tr>
                <td>
                    <label for=#{fvId emailView}>
                        Email (private, optional):
                <td>
                    ^{fvInput emailView}
            <tr>
                <td>
                    <label for=#{fvId avatarView}>
                        Avatar (link, optional):
                <td>
                    ^{fvInput avatarView}
            <tr>
                <td>
                    <label for=#{fvId nickView}>
                        IRC Nick (irc.freenode.net, optional):
                <td>
                    ^{fvInput nickView}
    |]

        passwdRes = case (passwd1Res, passwd2Res) of
            (FormSuccess a, FormSuccess b) -> if a == b then FormSuccess a else FormFailure ["passwords do not match"]
            (FormSuccess _, x) -> x
            (x, _) -> x

        result = (,,,,,) <$> identRes <*> passwdRes <*> nameRes
                         <*> emailRes <*> avatarRes <*> nickRes

    return (result, view)

editUserForm :: Maybe User -> Form UserUpdate
editUserForm muser = renderBootstrap3 BootstrapBasicForm $
    UserUpdate
        <$> aopt' textField               "Public Name"                                    (userName                      <$> muser)
        <*> aopt' textField               "Avatar image (link)"                            (userAvatar                    <$> muser)
        <*> aopt' emailField              "Email"                                          (userEmail                     <$> muser)
        <*> aopt' textField               "IRC nick @freenode.net)"                        (userIrcNick                   <$> muser)
        <*> aopt' snowdriftMarkdownField  "Blurb (used on listings of many people)"        (userBlurb                     <$> muser)
        <*> aopt' snowdriftMarkdownField  "Personal Statement (visible only on this page)" (userStatement                 <$> muser)

changePasswordForm :: Form ChangePassword
changePasswordForm = renderBootstrap3 BootstrapBasicForm $ ChangePassword
    <$> areq' passwordField "Current password" Nothing
    <*> areq' passwordField "New password"     Nothing
    <*> areq' passwordField "Repeat"           Nothing

-- | Form to mark a user as eligible for establishment. The user is fully established
-- when s/he accepts the honor pledge.
establishUserForm :: Form Text
establishUserForm = renderBootstrap3 BootstrapBasicForm $ areq' textField "Reason" Nothing

previewUserForm :: User -> Form UserUpdate
previewUserForm User{..} = renderBootstrap3 BootstrapBasicForm $
    UserUpdate
        <$> aopt hiddenField "" (Just userName)
        <*> aopt hiddenField "" (Just userAvatar)
        <*> aopt hiddenField "" (Just userEmail)
        <*> aopt hiddenField "" (Just userIrcNick)
        <*> hiddenMarkdown userBlurb
        <*> hiddenMarkdown userStatement

-- | Render a User profile, including
renderUser :: Maybe UserId -> UserId -> User -> Map (Entity Project) (Set Role) -> Widget
renderUser mviewer_id user_id user projects_and_roles = do
    let user_entity = Entity user_id user

    should_show_est_form <- handlerToWidget (canCurUserMakeEligible user_id)
    mest_form_and_enctype <-
        if should_show_est_form
            then Just <$> handlerToWidget (generateFormPost establishUserForm)
            else return Nothing

    $(widgetFile "user")

setPasswordForm :: Form SetPassword
setPasswordForm = renderBootstrap3 BootstrapBasicForm $ SetPassword
    <$> areq' passwordField "New password" Nothing
    <*> areq' passwordField "Repeat"       Nothing

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

userNotificationsForm :: Bool
                      -> Maybe (NonEmpty NotificationDelivery)
                      -> Maybe (NonEmpty NotificationDelivery)
                      -> Maybe (NonEmpty NotificationDelivery)
                      -> Maybe (NonEmpty NotificationDelivery)
                      -> Maybe (NonEmpty NotificationDelivery)
                      -> Maybe (NonEmpty NotificationDelivery)
                      -> Maybe (NonEmpty NotificationDelivery)
                      -> Form NotificationPref
userNotificationsForm is_moderator mbal mucom mrcom mrep mecon mflag mflagr =
    renderBootstrap3 (BootstrapHorizontalForm (ColSm 0) (ColSm 0) (ColSm 0) (ColSm 0)) $ NotificationPref
        <$> req "You have a low balance (less than 3 months funds at current pledge levels)"
                                               mbal
        <*> unapproved_comment
        <*> opt "Your comment gets rethreaded/moved"           mrcom
        <*> opt "Reply posted to watched thread"                        mrep
        <*> req "Your wiki post has an edit conflict"                mecon
        <*> req "Your comment gets flagged"              mflag
        <*> opt "A comment you flagged gets reposted" mflagr
  where
    unapproved_comment =
        if is_moderator
            then Just <$> req "An new comment awaits moderator approval" mucom
            else pure Nothing
    -- 'checkboxesFieldList' does not allow to work with 'NonEmpty'
    -- lists, so we have to work around that.
    req s xs = N.fromList <$> areq checkboxes s (N.toList <$> xs)
    opt s xs = fmap N.fromList <$> aopt checkboxes s (Just <$> N.toList <$> xs)
    checkboxes = checkboxesFieldList methods
    methods :: [(Text, NotificationDelivery)]
    methods =
        -- XXX: Support 'NotifDeliverEmailDigest'.
        [ ("website", NotifDeliverWebsite)
        , ("email",   NotifDeliverEmail)
        ]
