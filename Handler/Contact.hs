module Handler.Contact where

import Import

import Model.Message
import Widgets.Markdown

contactForm :: Form Markdown
contactForm = renderBootstrap3 $ areq' snowdriftMarkdownField "" Nothing

getContactR :: Text -> Handler Html
getContactR project_handle = do
    (contact_form, _) <- generateFormPost contactForm
    Entity _ project <- runYDB $ getBy404 (UniqueProjectHandle project_handle)
    defaultLayout $ do
        setTitle . toHtml $ "Contact " <> projectName project <> " | Snowdrift.coop"
        $(widgetFile "contact")


postContactR :: Text -> Handler Html
postContactR project_handle = do
    maybe_user_id <- maybeAuthId

    ((result, _), _) <- runFormPost contactForm

    case result of
        FormSuccess content -> do
            runSYDB $ do
                Entity project_id _ <- lift $ getBy404 $ UniqueProjectHandle project_handle
                insertMessage_ MessageDirect (Just project_id) maybe_user_id Nothing content False

            alertSuccess "Comment submitted.  Thank you for your input!"

        _ -> alertDanger "Error occurred when submitting form."

    redirect $ ContactR project_handle

