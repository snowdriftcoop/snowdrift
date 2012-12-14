module Handler.Contact where

import Import

import Widgets.Sidebar


contactForm :: Form Textarea
contactForm = renderDivs $ areq textareaField "" Nothing

getContactR :: Handler RepHtml
getContactR = do
    (contact_form, _) <- generateFormPost contactForm
    defaultLayout $(widgetFile "contact")


postContactR :: Handler RepHtml
postContactR = do
    user_id <- requireAuthId
    now <- liftIO getCurrentTime

    ((result, _), _) <- runFormPost contactForm

    case result of
        FormSuccess content -> do
            _ <- runDB $ insert $ Message now user_id Nothing content
            setMessage "Comment submitted.  Thank you for your input!"

        _ -> do
            setMessage "Error occurred when submitting form."

    redirect ContactR

