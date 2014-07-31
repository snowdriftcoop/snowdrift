module View.User
    ( createUserForm
    , editUserForm
    , establishUserForm
    , previewUserForm
    , renderUser
    ) where

import Import

import           Model.Markdown
import           Model.Role
import           Model.User
import           Widgets.Markdown       (snowdriftMarkdownField)
import           Widgets.ProjectPledges

import qualified Data.Map               as M
import qualified Data.Set               as S
import           Yesod.Markdown

createUserForm :: Maybe Text -> Form (Text, Text, Maybe Text, Maybe Text, Maybe Text)
createUserForm ident extra = do
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

editUserForm :: Maybe User -> Form UserUpdate
editUserForm muser = renderBootstrap3 $
    UserUpdate
        <$> aopt' textField               "Public Name"                                    (userName                      <$> muser)
        <*> aopt' textField               "Avatar image (link)"                            (userAvatar                    <$> muser)
        <*> aopt' textField               "IRC nick @freenode.net)"                        (userIrcNick                   <$> muser)
        <*> aopt' snowdriftMarkdownField  "Blurb (used on listings of many people)"        (userBlurb                     <$> muser)
        <*> aopt' snowdriftMarkdownField  "Personal Statement (visible only on this page)" (userStatement                 <$> muser)
        -- <*> aopt' messagePreferencesField "Message filter"                                 (Just . userMessagePreferences <$> muser)

-- | Form to mark a user as eligible for establishment. The user is fully established
-- when s/he accepts the honor pledge.
establishUserForm :: Form Text
establishUserForm = renderBootstrap3 $ areq' textField "Reason" Nothing

previewUserForm :: User -> Form UserUpdate
previewUserForm User{..} = renderBootstrap3 $
    UserUpdate
        <$> aopt hiddenField "" (Just userName)
        <*> aopt hiddenField "" (Just userAvatar)
        <*> aopt hiddenField "" (Just userIrcNick)
        <*> hiddenMarkdown userBlurb
        <*> hiddenMarkdown userStatement
        -- <*> aopt messagePreferencesField "" (Just (Just userMessagePreferences))

-- messagePreferencesField :: Field Handler [MessagePreference]
-- messagePreferencesField = checkboxesFieldList (map (showMessagePreference &&& id) [minBound..maxBound])

-- | Render a User profile, including
renderUser :: Maybe UserId -> UserId -> User -> Map (Entity Project) (Set Role) -> Widget
renderUser mviewer_id user_id user projects_and_roles = do
    let user_entity = Entity user_id user
        project_handle = error "bad link - no default project on user pages" -- TODO turn this into a caught exception

    should_show_est_form <- handlerToWidget (canCurUserMakeEligible user_id)
    mest_form_and_enctype <-
        if should_show_est_form
            then Just <$> handlerToWidget (generateFormPost establishUserForm)
            else return Nothing

    $(widgetFile "user")

hiddenMarkdown :: Maybe Markdown -> AForm Handler (Maybe Markdown)
hiddenMarkdown Nothing               = fmap (fmap Markdown) $ aopt hiddenField "" Nothing
hiddenMarkdown (Just (Markdown str)) = fmap (fmap Markdown) $ aopt hiddenField "" (Just $ Just str)
