module Handler.Contact where

import Import

import Widgets.Sidebar


contactForm :: Form Textarea
contactForm = renderDivs $ areq textareaField "" Nothing

getContactR :: Text -> Handler Html
getContactR project_handle = do
    (contact_form, _) <- generateFormPost contactForm
    defaultLayout $(widgetFile "contact")


postContactR :: Text -> Handler Html
postContactR project_handle = do
    maybe_user_id <- maybeAuthId
    now <- liftIO getCurrentTime

    ((result, _), _) <- runFormPost contactForm

    case result of
        FormSuccess content -> do
            void $ runDB $ insert $ Message now maybe_user_id Nothing content
            setMessage "Comment submitted.  Thank you for your input!"

        _ -> setMessage "Error occurred when submitting form."

    redirect $ ContactR project_handle

