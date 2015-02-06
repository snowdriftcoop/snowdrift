module Handler.Project.Signup where

import Import

import Model.License       (fetchLicensesDB)
import View.Project.Signup (projectSignupForm)

getProjectSignupR :: Handler Html
getProjectSignupR = do
    licenses <- runDB fetchLicensesDB
    (project_signup_form, _) <- generateFormPost $ projectSignupForm licenses
    defaultLayout $ do
        setTitle "Project sign up | Snowdrift.coop"
        $(widgetFile "project_signup")

postProjectSignupR :: Handler Html
postProjectSignupR = do
    licenses <- runDB fetchLicensesDB
    ((result, project_signup_form), _) <- runFormPost $ projectSignupForm licenses
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
