-- | This module should not live beyond January 2016. PLEASE PLEASE SHOOT
-- IT DEAD AFTER THAT DATE.
--
-- This module is just a placeholder before fleshing out the modules needed
-- for the alpha sprint
module Handler.NewDesign where

import Import

-- | Using explicit imports for now. It feels good to treat existing code
-- as a 3rd-party library.
import Dev
import Handler.TH
import Handler.User.Utils (startEmailVerification)
import Handler.Utils
import Model.Project ( fetchPublicProjectsDB)
import Model.User
        ( fetchUserProjectsAndRolesDB
        , userDisplayName
        )
import View.User (renderUser, createUserForm)

getWelcomeR :: Handler Html
getWelcomeR = defaultLayoutNew "homepage" $ do
    setTitle "Snowdrift.coop — Free the Commons"
    $(widgetFile "homepage")

getSearchR :: Handler Html
getSearchR = do
    q <- lookupGetParam "q"
    $(widget "search" "Search")

getPUpdatesR,
    getPTransactionsR
    :: Text -> Handler Html

getPUpdatesR handle =
    defaultLayoutNew "project/updates" $ do
        snowdriftTitle (handle <> ": Updates")
        projectNav handle
        $(widgetFile "project/updates")
getPTransactionsR handle =
    defaultLayoutNew "project/transactions" $ do
        snowdriftTitle (handle <> ": Transactions")
        projectNav handle
        $(widgetFile "project/transactions")

--
-- #### Helper functions for projects
--

projectNav :: ProjectHandle -> Widget
projectNav handle =
    [whamlet|
        <h3>Subpages
        <ul>
            <li><a href=@{PUpdatesR handle}>Updates
            <li><a href=@{PTransactionsR handle}>Transactions
    |]


--
-- #### Dashboard and Homepage
--

dashboardNav :: Widget
dashboardNav = do
    uid <- handlerToWidget requireAuthId
    $(widgetFile "dashboard/nav")

getHomeR,
    getUTransactionsR,
    getUPledgesR,
    getURolesR,
    getUEditR
    :: Handler Html

-- | Homepage is an introduction to the site for non-logged-in viewers, and
-- the dashboard for logged-in viewers.
getHomeR = do
    u <- maybeAuth
    maybe (defaultLayoutNew "homepage" $ do
              setTitle "Snowdrift.coop — Free the Commons"
              $(widgetFile "homepage"))
          (\user ->
              $(widget "dashboard/overview" "Dashboard"))
          u

getUTransactionsR = do
    user <- requireAuth
    $(widget "dashboard/transactions" "Transactions")
getUPledgesR = do
    user <- requireAuth
    $(widget "dashboard/pledges" "Pledges")
getURolesR = do
    user <- requireAuth
    $(widget "dashboard/roles" "My Project Roles")
getUEditR = do
    user <- requireAuth
    $(widget "dashboard/edit-profile" "Edit Profile")

--
-- #### NEEDS REVIEW. COPIED FROM EXISTING PAGES.
--

-- | Projects list.
getProjectsR :: Handler Html
getProjectsR = do
    projects <- runDB fetchPublicProjectsDB
    $(widget "projects" "Projects")

-- | Public page for a project
getPHomeR :: ProjectHandle -> Handler Html
getPHomeR handle = do
    Entity _ project <- runDB $ getBy404 $ UniqueProjectHandle handle

    defaultLayoutNew "project/home" $ do
        snowdriftTitle $ projectName project
        $(widgetFile "project/home")

-- | The new account page using hashdb (passphrase-based) authentication.
getCreateAccountR :: Handler Html
getCreateAccountR = do
    (form, _) <- generateFormPost $ createUserForm Nothing
    defaultLayoutNew "create-account" $ do
        snowdriftTitle "Free the Commons"
        [whamlet|
            ^{alphaRewriteNotice}
            <form method=POST>
                ^{form}
                <input type=submit>
        |]

-- | Handles form posting for a user signing up.
postCreateAccountR :: Handler Html
postCreateAccountR = do
    ((result, form), _) <- runFormPost $ createUserForm Nothing

    case result of
        FormSuccess (ident, passph, name, memail, avatar, nick) -> do
            muser_id <-
                createUser ident
                           (Just passph)
                           name
                           (NewEmail False <$> memail)
                           avatar
                           nick
            fromMaybe (pure())
                      (startEmailVerification <$> muser_id <*> memail)
            case muser_id of
                Nothing -> do
                    alertDanger
                        "There was an error creating your account. Please try again."
                    redirect CreateAccountR
                Just _ -> do
                    setCreds True (Creds "hashdb" ident [])
                    redirectUltDest HomeR

        FormMissing -> alertDanger "missing field"
        FormFailure strings -> alertDanger (mconcat strings)

    defaultLayout $ [whamlet|
        <form method=POST>
            ^{form}
            <button type=submit>Submit
    |]

-- | Public profile for a user.
getUserR :: UserId -> Handler Html
getUserR user_id = do
    mviewer_id <- maybeAuthId

    user <- runYDB $ get404 user_id

    projects_and_roles <- runDB (fetchUserProjectsAndRolesDB user_id)
    when ( Just user_id == mviewer_id
        && isJust (userEmail user)
        && not (userEmail_verified user)
        ) $ alertWarning $
              "Email address is not verified. Until you verify it, "
              <> "you will not be able to receive email notifications."

    defaultLayoutNew "user" $ do
        snowdriftDashTitle "User Profile" $
            userDisplayName (Entity user_id user)
        alphaRewriteNotice
        renderUser mviewer_id user_id user projects_and_roles
