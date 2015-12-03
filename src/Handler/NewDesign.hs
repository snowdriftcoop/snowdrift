-- | This module should not live beyond January 2016. PLEASE PLEASE SHOOT
-- IT DEAD AFTER THAT DATE.
--
-- This module is just a placeholder before fleshing out the modules needed
-- for the alpha sprint
module Handler.NewDesign where

import Import

import Handler.TH
import Handler.Utils
import Model.Count
import Model.License (fetchLicensesDB)
import Model.Project
import Model.User
import View.Project
import View.Project.Signup (projectSignupForm)
import qualified Mechanism as Mech

getSearchR,
    getUSignupR,
    postUSignupR,
    getUTransactionsR,
    getUNoticesR,
    getUPledgesR,
    getUMembershipR,
    getUEditR
    :: Handler Html

getSearchR        = $(simpleHandler "search" "Search")
getUSignupR       = $(simpleHandler "signup" "Signup")
postUSignupR      = $(simpleHandler "post-signup" "Signup")
getUTransactionsR = $(simpleHandler "transactions" "Transactions")
getUNoticesR      = $(simpleHandler "notices" "Notices")
getUPledgesR      = $(simpleHandler "pledges" "Pledges")
getUMembershipR   = $(simpleHandler "memberships" "Project Memberships")
getUEditR         = $(simpleHandler "edit-profile" "Edit Profile")

getPUpdatesR,
    getPTransactionsR
    :: Text -> Handler Html

getPUpdatesR handle =
    defaultLayoutNew "project/updates" $ do
        snowdriftTitle (handle <> ": Updates")
        $(widgetFile "project/updates")
getPTransactionsR handle =
    defaultLayoutNew "project/transactions" $ do
        snowdriftTitle (handle <> ": Transactions")
        $(widgetFile "project/transactions")


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
        is_watching <- case mviewer_id of
            Nothing -> return False
            Just viewer_id -> userIsWatchingProjectDB viewer_id project_id
        return (project_id, project, is_watching)

    defaultLayoutNew "project/home" $ do
        snowdriftTitle $ projectName project
        [whamlet|
            <h3>Subpages
            <ul>
                <li><a href=@{ProjectR handle PUpdatesR}>Updates
                <li><a href=@{WikiPagesR handle}>Wiki</a> (links to existing)
                <li><a href=@{ProjectDiscussionR handle}>Discussion</a> (links to existing)
                <li><a href=@{ProjectR handle PTransactionsR}>Transactions
        |]
        renderProject (Just project_id) project mviewer_id is_watching
