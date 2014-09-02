module Handler.Contact where

import Import

import Widgets.Markdown

import Model.User
import Model.Comment

contactForm :: Form Markdown
contactForm = renderBootstrap3 $ areq' snowdriftMarkdownField "" Nothing

getProjectContactR :: Text -> Handler Html
getProjectContactR project_handle = do
    (contact_form, _) <- generateFormPost contactForm
    Entity _ project <- runYDB $ getBy404 (UniqueProjectHandle project_handle)
    defaultLayout $ do
        setTitle . toHtml $ "Contact " <> projectName project <> " | Snowdrift.coop"
        $(widgetFile "contact")


postProjectContactR :: Text -> Handler Html
postProjectContactR project_handle = do
    maybe_user_id <- maybeAuthId

    ((result, _), _) <- runFormPost contactForm

    Entity _ project <- runYDB $ getBy404 (UniqueProjectHandle project_handle)

    case result of
        FormSuccess content -> do
            _ <- runSDB (postApprovedCommentDB (fromMaybe anonymousUser maybe_user_id) Nothing (projectDiscussion project) content VisPrivate)

            alertSuccess "Comment submitted.  Thank you for your input!"

        _ -> alertDanger "Error occurred when submitting form."

    redirect $ ProjectContactR project_handle

