module View.Project
    ( editProjectForm
    , projectContactForm
    , inviteForm
    , projectBlogForm
    , projectConfirmSharesForm
    , renderBlogPost
    , renderProject
    , viewForm
    ) where


import Import

import           Data.Filter
import           Data.Maybe (fromJust)
import           Data.Order
import           Model.Currency
import           Model.Discussion
import           Model.Markdown
import           Model.Project
import           Model.Shares
import           Model.Role
import           View.User (userNameWidget)
import           Widgets.Markdown
import           Widgets.Time

import qualified Data.Text       as T
import           Data.Time.Clock
import           Yesod.Markdown

renderProject :: Maybe ProjectId -> Project -> Bool -> [Int64]
              -> Maybe (Entity Pledge) -> WidgetT App IO ()
renderProject maybe_project_id project is_watching pledges pledge = do
    let share_value = projectShareValue project
        users = fromIntegral $ length pledges
        shares = sum pledges
        project_value = share_value $* fromIntegral shares
        discussion = DiscussionOnProject $ Entity (fromMaybe (Key $ PersistInt64 (-1)) maybe_project_id) project
        description = markdownWidgetWith (fixLinks (projectHandle project) discussion) $ projectDescription project

        maybe_shares = pledgeShares . entityVal <$> pledge

    now <- liftIO getCurrentTime

    amounts <- case projectLastPayday project of
        Nothing -> return Nothing
        Just last_payday -> handlerToWidget $ runDB $ do
            -- This assumes there were transactions associated with the last payday
            [Value (Just last) :: Value (Maybe Rational)] <-
                select $
                from $ \ transaction -> do
                where_ $
                    transaction ^. TransactionPayday ==. val (Just last_payday) &&.
                    transaction ^. TransactionCredit ==. val (Just $ projectAccount project)
                return $ sum_ $ transaction ^. TransactionAmount

            [Value (Just year) :: Value (Maybe Rational)] <-
                select $
                from $ \ (transaction `InnerJoin` payday) -> do
                where_ $
                    payday ^. PaydayDate >. val (addUTCTime (-365 * 24 * 60 * 60) now) &&.
                    transaction ^. TransactionCredit ==. val (Just $ projectAccount project)
                on_ $ transaction ^. TransactionPayday ==. just (payday ^. PaydayId)
                return $ sum_ $ transaction ^. TransactionAmount

            [Value (Just total) :: Value (Maybe Rational)] <-
                select $
                from $ \ transaction -> do
                where_ $ transaction ^. TransactionCredit ==. val (Just $ projectAccount project)
                return $ sum_ $ transaction ^. TransactionAmount

            return $ Just (Milray $ round last, Milray $ round year, Milray $ round total)


    ((_, update_shares), _) <- handlerToWidget $ generateFormGet $ maybe previewPledgeForm pledgeForm maybe_project_id

    $(widgetFile "project")

renderBlogPost :: Text -> BlogPost -> WidgetT App IO ()
renderBlogPost project_handle blog_post = do
    project <- handlerToWidget $ runYDB $ getBy404 $ UniqueProjectHandle project_handle

    let (Markdown top_content) = blogPostTopContent blog_post
        (Markdown bottom_content) = maybe (Markdown "") id $ blogPostBottomContent blog_post
        title = blogPostTitle blog_post
        author = blogPostUser blog_post
        discussion = DiscussionOnProject project
        content = markdownWidgetWith (fixLinks project_handle discussion) $ Markdown $ top_content <> "\n\n***\n\n<a name=\"fold\"></a>\n\n" <> bottom_content

    $(widgetFile "blog_post")

editProjectForm :: Maybe (Project, [Text]) -> Form UpdateProject
editProjectForm project =
    renderBootstrap3 BootstrapBasicForm $ UpdateProject
        <$> areq' textField "Project Name" (projectName . fst <$> project)
        <*> areq' snowdriftMarkdownField "Description" (projectDescription . fst <$> project)
        <*> (maybe [] (map T.strip . T.splitOn ",") <$> aopt' textField "Tags" (Just . T.intercalate ", " . snd <$> project))
        <*> aopt' textField "Github Repository" (projectGithubRepo . fst <$> project)

projectBlogForm :: Maybe (Text, Text, Markdown) -> Form (Text, Text, Markdown)
projectBlogForm defaults = renderBootstrap3 BootstrapBasicForm $
    let getTitle (title, _, _) = title
        getHandle (_, handle, _) = handle
        getContent (_, _, content) = content
     in (,,)
        <$> areq' textField "Title for this blog post" (getTitle <$> defaults)
        <*> areq' textField "Handle for the URL" (getHandle <$> defaults)
        <*> areq' snowdriftMarkdownField "Content" (getContent <$> defaults)

projectContactForm :: Form (Markdown, Language)
projectContactForm = renderBootstrap3 BootstrapBasicForm $ (,)
    <$> areq' snowdriftMarkdownField "" Nothing
    <*> areq' (selectField makeLanguageOptions) "Language" Nothing

inviteForm :: Form (Text, Role)
inviteForm = renderBootstrap3 BootstrapBasicForm $ (,)
    <$> areq' textField "About this invitation:" Nothing
    <*> areq roleField "Type of Invite:" (Just TeamMember)

viewForm :: Form (Filterable -> Bool, Orderable -> [Double])
viewForm = renderBootstrap3 BootstrapBasicForm $ (,)
    <$> (either (const defaultFilter) id . parseFilterExpression . fromMaybe "" <$> aopt' textField "filter" Nothing)
    <*> (either (const defaultOrder) id . parseOrderExpression . fromMaybe "" <$> aopt' textField "sort" Nothing)

projectConfirmSharesForm :: Maybe Int64 -> Form SharesPurchaseOrder
projectConfirmSharesForm = renderBootstrap3 BootstrapBasicForm . fmap SharesPurchaseOrder . areq' hiddenField ""
