{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

module View.Project
    ( editProjectForm
    , projectContactForm
    , inviteForm
    , ProjectBlog (..)
    , projectBlogForm
    , projectConfirmSharesForm
    , previewBlogPost
    , renderBlogPost
    , renderProject
    , viewForm
    ) where


import Import

import           Data.Filter
import           Data.Maybe (fromJust)
import           Data.Order
import           Model.Currency
import           Model.Markdown
import           Model.Project
import           Model.Shares
import           Model.Role
import           View.User (userNameWidget)
import           Widgets.Markdown
import           Widgets.Preview
import           Widgets.Time

import qualified Data.Text       as T
import           Data.Time.Clock

renderProject :: Maybe ProjectId -> Project -> Maybe UserId -> Bool -> [Int64]
              -> Maybe (Entity Pledge) -> WidgetT App IO ()
renderProject maybe_project_id project mviewer_id is_watching pledges pledge = do
    let share_value = projectShareValue project
        users = fromIntegral $ length pledges
        shares = sum pledges
        project_value = share_value $* fromIntegral shares
        discussion = DiscussionOnProject $ Entity (fromMaybe (key $ PersistInt64 (-1)) maybe_project_id) project
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

previewBlogPost :: UserId -> Text -> ProjectBlog -> Handler Html
previewBlogPost viewer_id project_handle project_blog@ProjectBlog {..} = do
    now <- liftIO getCurrentTime
    Entity project_id _ <- runYDB $ getBy404 $ UniqueProjectHandle project_handle
    let (top_content', bottom_content') = break (== "***") $ T.lines $
                                              unMarkdown projectBlogContent
        top_content = T.unlines top_content'
        bottom_content = if null bottom_content'
                             then Nothing
                             else Just $ Markdown $ T.unlines bottom_content'
        blog_post = BlogPost now projectBlogTitle projectBlogHandle
                             viewer_id project_id (key $ PersistInt64 0)
                             (Markdown top_content) bottom_content
    (form, _) <- generateFormPost $ projectBlogForm $ Just project_blog
    defaultLayout $ previewWidget form "post" $
        renderBlogPost project_handle blog_post

editProjectForm :: Maybe (Project, [Text]) -> Form UpdateProject
editProjectForm project =
    renderBootstrap3 BootstrapBasicForm $ UpdateProject
        <$> areq' textField "Project Name" (projectName . fst <$> project)
        <*> areq' snowdriftMarkdownField "Description" (projectDescription . fst <$> project)
        <*> (maybe [] (map T.strip . T.splitOn ",") <$> aopt' textField "Tags" (Just . T.intercalate ", " . snd <$> project))
        <*> aopt' textField "GitHub Repository (to show GH tickets here at Snowdrift.coop)" (projectGithubRepo . fst <$> project)

data ProjectBlog = ProjectBlog
    { projectBlogTitle   :: Text
    , projectBlogHandle  :: Text
    , projectBlogContent :: Markdown
    } deriving Show

projectBlogForm :: Maybe ProjectBlog -> Form ProjectBlog
projectBlogForm mproject_blog =
    renderBootstrap3 BootstrapBasicForm $ ProjectBlog
        <$> areq' textField "Title for this blog post"
                (projectBlogTitle <$> mproject_blog)
        <*> areq' textField "Handle for the URL"
                (projectBlogHandle <$> mproject_blog)
        <*> areq' snowdriftMarkdownField "Content"
                (projectBlogContent <$> mproject_blog)

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
projectConfirmSharesForm = renderBootstrap3 BootstrapBasicForm . fmap SharesPurchaseOrder . areq hiddenField ""
