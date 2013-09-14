module Handler.Contact where

import Import

import Widgets.Sidebar
import Widgets.Markdown


contactForm :: Form Markdown
contactForm = renderDivs $ areq snowdriftMarkdownField "" Nothing

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
            void $ runDB $ do
                Entity project_id _ <- getBy404 $ UniqueProjectHandle project_handle
                insert $ Message (Just project_id) now maybe_user_id Nothing content

            setMessage "Comment submitted.  Thank you for your input!"

        _ -> setMessage "Error occurred when submitting form."

    redirect $ ContactR project_handle

