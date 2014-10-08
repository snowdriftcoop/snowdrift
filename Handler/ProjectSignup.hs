module Handler.ProjectSignup where

import Import

import Model.ProjectSignup
import Model.License

projectSignupForm :: UserId -> Html -> MForm Handler (FormResult ProjectSignup, Widget)
projectSignupForm user_id _ = do
    licenses <- lift $ runDB getLicenses

    (projectNameRes, projectNameView) <- mreq textField (generateFieldSettings "Project Name" [("class", "form-control"), ("placeholder", "Project Name")]) Nothing
    (projectHandleRes, projectHandleView) <- mreq textField (generateFieldSettings "Project Handle" [("class", "form-control")]) Nothing
    (projectWebsiteRes, projectWebsiteView) <- mopt textField (generateFieldSettings "Website" [("class", "form-control"), ("placeholder", "Project Website")]) Nothing
    (projectTypeRes, projectTypeView) <- mreq (multiSelectFieldList getProjectTypes) (generateFieldSettings "Project Type" [("class", "form-control"), ("placeholder", "Project Type(s)")]) Nothing
    (projectTypeOtherRes, projectTypeOtherView) <- mopt textField (generateFieldSettings "ProjectTypeOther" [("class", "form-control"), ("placeholder", "Describe Other Project Type")]) Nothing
    (projectLicenseRes, projectLicenseView) <- mreq (multiSelectFieldList $ getLicenseLabels licenses) (generateFieldSettings "License" [("class", "form-control"), ("placeholder", "Select License(s)")]) Nothing
    (projectLicenseOtherRes, projectLicenseOtherView) <- mopt textField (generateFieldSettings "ProjectLicenseOther" [("class", "form-control"), ("placeholder", "Describe Other License Type")]) Nothing
    (projectHistoryRes, projectHistoryView) <- mreq textField (generateFieldSettings "ProjectHistory" [("class", "form-control"), ("placeholder", "Length of time project has been active")]) Nothing
    (projectLocationRes, projectLocationView) <- mreq textField (generateFieldSettings "Location" [("class", "form-control"), ("placeholder", "Location Project is legally based out of")]) Nothing
    (projectLegalStatusRes, projectLegalStatusView) <- mreq (selectFieldList getLegalStatuses) (generateFieldSettings "Legal Status" [("class", "form-control"), ("placeholder", "Legal Status of Project")]) Nothing
    (projectLegalStatusCommentsRes, projectLegalStatusCommentsView) <- mopt textField (generateFieldSettings "Legal Status Comments" [("class", "form-control"), ("placeholder", "Additional Comments about Legal Status")]) Nothing
    (projectUserRoleRes, projectUserRoleView) <- mreq textField (generateFieldSettings "User Role" [("class", "form-control"), ("placeholder", "Describe your role within the project")]) Nothing
    (projectMissionRes, projectMissionView) <- mreq textareaField (generateFieldSettings "Mission" [("class", "form-control"), ("placeholder", "Describe your project's vision and mission")]) Nothing
    (projectGoalsRes, projectGoalsView) <- mreq textareaField (generateFieldSettings "Goals" [("class", "form-control"), ("placeholder", "Describe your project's short-term and long-term goals and deliverables")]) Nothing
    (projectFundsRes, projectFundsView) <- mreq textareaField (generateFieldSettings "Funds" [("class", "form-control"), ("placeholder", "Describe how your project will benefit from and make use of funds raised through Snowdrift.coop")]) Nothing
    (projectOtherInfoRes, projectOtherInfoView) <- mopt textareaField (generateFieldSettings "Other Info" [("class", "form-control"), ("placeholder", "Please provide any additional information you find pertinent here (e.g. other contacts affiliated with the project and their contact info)")]) Nothing

    let projectSignupRes = ProjectSignup 
                <$> projectNameRes
                <*> projectHandleRes
                <*> projectWebsiteRes
                <*> projectTypeRes
                <*> projectTypeOtherRes
                <*> projectLicenseRes
                <*> projectLicenseOtherRes
                <*> projectHistoryRes
                <*> projectLocationRes
                <*> projectLegalStatusRes
                <*> projectLegalStatusCommentsRes
                <*> projectUserRoleRes
                <*> projectMissionRes
                <*> projectGoalsRes 
                <*> projectFundsRes
                <*> projectOtherInfoRes
                <*> pure user_id
                <*> pure newProjectSignupStatus
    let widget = toWidget $(widgetFile "project_signup")
    return (projectSignupRes, widget)


getProjectSignupR :: Handler Html
getProjectSignupR = do
    user_id <- requireAuthId
    ((_, widget), enctype) <- runFormGet (projectSignupForm user_id)
    defaultLayout $ do
        setTitle "Project Signup Form | Snowdrift.coop"
        [whamlet|
            <H1 align="center">
                Snowdrift.coop
            <H3 align="center">
                Project SignUp Form
            <form method=POST action=@{ProjectSignupR} enctype=#{enctype}>
                ^{widget}
                <button>Submit Application
        |]

postProjectSignupR :: Handler Html
postProjectSignupR = do
    user_id <- requireAuthId
    ((result, widget), enctype) <- runFormPostNoToken (projectSignupForm user_id)
    case result of
        FormSuccess project -> do
          [ Value projectcheck :: Value Int64 ] <- runDB $ select $ from $ \a -> do
              where_ (a ^. ProjectHandle ==. val (projectSignupHandle project))
              return $ count (a ^. ProjectHandle)
              
          projectsubmitted <- if projectcheck > 0
                                then return Nothing
                                else runDB $ insertUnique project

          if isJust projectsubmitted
              then defaultLayout $ do
                      setTitle "Project Signup Form: Success! | Snowdrift.coop"
                      [whamlet|
                          <H1 Align=Center>Success!
                          <br>
                          <p>Your application to add #{projectSignupName $ project} has been submitted for review by the Snowdrift administrators to validate that your project meets the standards for Snowdrift.  You will receive a response within 7 - 10 business days.
                          <p>If your application is approved, your project's default Snowdrift page will be created for you, at which point you can edit the content as you see fit.
                          <p>If your application is rejected, you will receive an e-mail explaining why your project does not meet Snowdrift's criteria.  
                          <H3 Align=Center>Thank you for your interest in Snowdrift!!
                          <form method=GET action=@{HomeR} enctype=#{enctype}>
                              <button>Return Home
                      |]
              
              else defaultLayout $ do
                  setTitle "Project Signup Form: Submission Error! | Snowdrift.coop"
                  [whamlet|
                      <H1 Align=Center>Submission Error
                      <ul>
                          <li color=red>#{projectSignupName $ project} already exists on Snowdrift.  You may have already submitted a sign-up request, which is still under review.
                      <form method=GET action=@{ProjectSignupR} enctype=#{enctype}>
                          <button>Return to previous form
                  |]

        FormFailure messages -> defaultLayout $ do
                setTitle "Project Signup Form: Submission Error! | Snowdrift.coop"
                [whamlet|
                    <H1 Align=Center>Submission Error
                    <ul>
                        $forall message <- messages
                            <li color=red>#{message}
                    <br>
                    <form .form-horizontal method=POST action=@{ProjectSignupR} enctype=#{enctype}>
                        ^{widget}
                        <button>Submit Application
                |]
                
        FormMissing -> defaultLayout $ do
                setTitle "Project Signup Form | Snowdrift.coop"
                [whamlet|
                <form method=POST enctype=#{enctype}>
                    ^{widget}
                    <button>Submit Application
                |]

applicationReviewForm :: Entity ProjectSignup -> Html -> MForm Handler (FormResult ReviewerComments, Widget)
applicationReviewForm project _ = do
    (reviewerCommentRes, reviewerCommentView) <- mopt textField (generateFieldSettings "Reviewer Comment" [("class", "form-control"), ("placecholder", "Enter Comments Here")]) Nothing 
    (reviewerStatusRes, reviewerStatusView) <- mreq (selectFieldList projectSignupStatus) (generateFieldSettings "Reviewer Status" [("class", "form-control")]) Nothing

    let reviewerCommentRes = ReviewerComment
        <$> pure $ ProjectSignupId project
        <*> lift (liftIO getCurrentTime)
        <*> reviewerCommentRes
        <*> reviewerStatusRes
        <*> 

    let widget = toWidget $(widgetFile "application_review")
    defaultLayout $ do
        setTitle "Application Review: #{ProjectSignupName project} | Snowdrift.coop"
        ^{widget}
        <button>Update

getProjectApplicationR :: ProjectSignupId -> Handler Html
getProjectApplicationR = do
    user_id <- requireAuthId
    if(userIsProjectAdminDB user_id (projectId getSiteProject))
        then
            project <- runDB $ select $ from \p -> do
                where_ (p ^. ProjectSignupId ==. val (ps_id))
                return p
            ((_, widget), enctype) <- runFormGet project

getProjectApplicationsR :: Handler Html
getProjectApplicationsR = do
    user_id <- requireAuthId
    project <- runDB $ select $ from \p -> do
        where_ (p ^. ProjectSignupStatus ==. val (InReview))
        limit 1
        return p
    ((_, widget), enctype) <- runFormGet project
