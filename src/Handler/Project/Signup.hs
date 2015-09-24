module Handler.Project.Signup where

import Import

import Model.License       (fetchLicensesDB)
import View.Project.Signup (projectSignupForm)

getProjectSignupR :: Handler Html
getProjectSignupR = do
    licenses <- runDB fetchLicensesDB
    render   <- getUrlRender
    (project_signup_form, _) <- generateFormPost $
        projectSignupForm render licenses
    defaultLayout $ do
        snowdriftTitle "Project Sign Up"
        $(widgetFile "project_signup")

postProjectSignupR :: Handler Html
postProjectSignupR = do
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
            defaultLayout $(widgetFile "project_signup")
        FormFailure _ -> do
            alertDanger "Form failure"
            defaultLayout $(widgetFile "project_signup")
