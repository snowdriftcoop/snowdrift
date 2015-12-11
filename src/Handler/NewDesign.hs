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
import Handler.Notification
        ( buildNotificationsList
        , Notification(..)
        )
import Handler.TH
import Handler.Utils
import Handler.User (startEmailVerification)
import Model.Count (getCount)
import Model.License (fetchLicensesDB)
import Model.Project
        ( fetchPublicProjectsDB
        , fetchProjectDiscussionsDB
        , fetchProjectOpenTicketsDB
        , projectNameWidget
        , summarizeProject
        , summaryDiscussionCount
        , summaryTicketCount
        )
import Model.User
        ( fetchProjectNotificationsDB
        , fetchUserNotificationsDB
        , fetchUserProjectsAndRolesDB
        , userDisplayName
        , userIsWatchingProjectDB
        , userReadNotificationsDB
        )
import View.Project (renderProject)
import View.Project.Signup (projectSignupForm)
import View.Time (renderTime)
import View.User (renderUser, createUserForm)
import qualified Mechanism as Mech

getSearchR, postCreateAccountR :: Handler Html
getSearchR         = $(simpleHandler "search" "Search")
postCreateAccountR = $(simpleHandler "auth/post-create-account" "Welcome")

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
            <li><a href=@{ProjectR handle PUpdatesR}>Updates
            <li><a href=@{WikiPagesR handle}>Wiki</a> (links to pre-alpha)
            <li><a href=@{ProjectDiscussionR handle}>Discussion</a> (links to pre-alpha)
            <li><a href=@{ProjectR handle PTransactionsR}>Transactions
    |]


--
-- #### Dashboard and Homepage
--

dashboardNav :: Widget
dashboardNav = $(widgetFile "dashboard/nav")

getHomeR,
    getUTransactionsR,
    getUPledgesR,
    getUMembershipsR,
    getUEditR
    :: Handler Html

-- | Homepage is an introduction to the site for non-logged-in viewers, and
-- the dashboard for logged-in viewers.
getHomeR = do
    u <- maybeAuth
    maybe $(simpleHandler "homepage" "Free the Commons")
          (\user ->
              $(simpleHandler "dashboard/overview" "Dashboard"))
          u

getUTransactionsR = do
    user <- requireAuth
    $(simpleHandler "dashboard/transactions" "Transactions")
getUPledgesR = do
    user <- requireAuth
    $(simpleHandler "dashboard/pledges" "Pledges")
getUMembershipsR = do
    user <- requireAuth
    $(simpleHandler "dashboard/memberships" "Project Memberships")
getUEditR = do
    user <- requireAuth
    $(simpleHandler "dashboard/edit-profile" "Edit Profile")

--
-- #### NEEDS REVIEW. COPIED FROM EXISTING PAGES.
--

-- | Where projects actually sign up.
--
-- As opposed to getPSignupR, where they learn about signing up. This page
-- will not be advertised during alpha.
getPSignupFormR :: Handler Html
getPSignupFormR = do
    licenses <- runDB fetchLicensesDB
    render   <- getUrlRender
    (project_signup_form, _) <- generateFormPost $
        projectSignupForm render licenses
    $(simpleHandler "project-signup-form" "Project Sign Up")

postPSignupFormR :: Handler Html
postPSignupFormR = do
    licenses <- runDB fetchLicensesDB
    render   <- getUrlRender
    ((result, project_signup_form), _) <- runFormPost $
        projectSignupForm render licenses
    case result of
        FormSuccess res  -> do
            runDB $ insert_ res
            alertSuccess "Application submitted"
            redirect HomeR
        FormMissing      -> do
            alertDanger "No data provided"
            $(simpleHandler "project-signup-form" "Project Sign Up")
        FormFailure _ -> do
            alertDanger "Form failure"
            $(simpleHandler "project-signup-form" "Project Sign Up")



-- | Projects list.
getProjectsR :: Handler Html
getProjectsR = do
    project_summaries <- runDB $ do
        projects <- fetchPublicProjectsDB
        forM projects $ \project -> do
            discussions <- fetchProjectDiscussionsDB $ entityKey project
            tickets <- fetchProjectOpenTicketsDB (entityKey project) Nothing
            let summary = summarizeProject project Mech.Project discussions tickets
            return (project, summary)

    let discussionsCount = getCount . summaryDiscussionCount
    let ticketsCount = getCount . summaryTicketCount

    $(simpleHandler "projects" "Projects")

-- | Public page for a project
getPHomeR :: ProjectHandle -> Handler Html
getPHomeR handle = do
    mviewer_id <- maybeAuthId

    (project_id, project, is_watching) <- runYDB $ do
        Entity project_id project <- getBy404 $ UniqueProjectHandle handle
        is_watching <- maybe (return False)
                             (`userIsWatchingProjectDB` project_id)
                             mviewer_id
        return (project_id, project, is_watching)

    defaultLayoutNew "project/home" $ do
        snowdriftTitle $ projectName project
        projectNav handle
        renderProject (Just project_id) project mviewer_id is_watching

-- | The account creation page for new users using our own (password-based)
-- authentication.
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
postUserCreateR :: Handler Html
postUserCreateR = do
    ((result, form), _) <- runFormPost $ createUserForm Nothing

    case result of
        FormSuccess (ident, passwd, name, memail, avatar, nick) -> do
            muser_id <-
                createUser ident
                           (Just passwd)
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

getUNotificationsR :: Handler Html
getUNotificationsR = do
    user_id <- requireAuthId
    notifs  <- runDB $ do
        userReadNotificationsDB user_id
        user_notifs    <- fetchUserNotificationsDB user_id
        project_notifs <- fetchProjectNotificationsDB user_id
        return $ buildNotificationsList user_notifs project_notifs
    $(simpleHandler "dashboard/notifications" "Notifications")
