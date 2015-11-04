{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

module View.Project
    ( editProjectForm
    , projectContactForm
    , inviteForm
    , Preview (..)
    , ProjectBlog (..)
    , projectBlogForm
    , projectConfirmPledgeForm
    , previewBlogPost
    , renderBlogPost
    , renderProject
    , viewForm
    ) where


import Import

import Control.Applicative (liftA2)
import qualified Data.Text as T

import Data.Filter
import Data.Order
import DeprecatedBootstrap
import Handler.Utils
import Model.Markdown
import Model.Project
import Model.Shares
import Model.User
import Model.Role
import Model.Comment
import View.Time
import View.User (userNameWidget)
import Widgets.Markdown
import Widgets.Preview
import qualified Mechanism as Mech

renderProject :: Maybe ProjectId
              -> Project
              -> Maybe UserId
              -> Bool
              -> WidgetT App IO ()
renderProject maybe_project_id project mviewer_id is_watching = do
    let discussion =
            DiscussionOnProject
                (Entity (fromMaybe (key $ PersistInt64 (-1)) maybe_project_id)
                        project)
        description =
            markdownWidgetWith (fixLinks (projectHandle project) discussion)
                               (projectDescription project)

    userIsAdmin <- case maybe_project_id of
        Just project_id ->
            maybe (pure False)
                  (\u -> runDB $ userIsProjectAdminDB u project_id) mviewer_id
        Nothing -> pure False

    now <- liftIO getCurrentTime

    amounts <- handlerToWidget (runDB (Mech.payoutHistory project now))

    ((_, update_pledge), _) <-
        handlerToWidget
            (generateFormGet
                (maybe previewPledgeForm pledgeForm maybe_project_id))

    (mechUser, _mechProject) <- runDB $ do
        u <- sequence $ fmap Mech.fetchUser mviewer_id
        p <- sequence $ fmap Mech.fetchProject maybe_project_id
        return (u,p)

    $(widgetFile "project")

data Preview = Preview | NotPreview deriving Eq

renderBlogPost :: Text -> BlogPost -> Preview -> WidgetT App IO ()
renderBlogPost project_handle blog_post preview = do
    (comment_count, project) <- handlerToWidget $ runYDB $ do
        project@(Entity project_id _) <-
            getBy404 $ UniqueProjectHandle project_handle

        comment_count <-
            fetchCommentCountDB Nothing
                                project_id
                                (blogPostDiscussion blog_post)
        return (comment_count, project)

    let (Markdown top_content) = blogPostTopContent blog_post
        (Markdown bottom_content) = fromMaybe (Markdown "")
                                              (blogPostBottomContent blog_post)
        title = blogPostTitle blog_post
        author = blogPostUser blog_post
        discussion = DiscussionOnProject project
        content =
            markdownWidgetWith
                (fixLinks project_handle discussion)
                (Markdown
                    (top_content <> "\n\n***\n\n<a name=\"fold\"></a>\n\n"
                                 <> bottom_content))

    $(widgetFile "blog_post")

previewBlogPost :: UserId -> Text -> ProjectBlog -> Handler Html
previewBlogPost viewer_id project_handle project_blog@ProjectBlog {..} = do
    now <- liftIO getCurrentTime
    Entity project_id _ <-
        runYDB $ getBy404 $ UniqueProjectHandle project_handle
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
        renderBlogPost project_handle blog_post Preview

editProjectForm :: Maybe (Project, [Text]) -> Form UpdateProject
editProjectForm mProjTags =
    renderBootstrap3 BootstrapBasicForm $ UpdateProject
        <$> areq' textField "Project Name"
            (projectName <$> mProj)
        <*> areq' textField "Blurb"
            (projectBlurb <$> mProj)
        <*> areq' snowdriftMarkdownField "Description"
            (projectDescription <$> mProj)
        <*> (maybe [] (map T.strip . T.splitOn ",") <$>
            aopt' textField "Tags"
                (Just . T.intercalate ", " <$> mTags))
        <*> aopt' textField
            "GitHub Repository (to show GH tickets here at Snowdrift.coop)"
            (projectGithubRepo <$> mProj)
        -- TODO: system to upload project logo as in SD-543
        -- the following <*> pure Nothing line inserts default logo for now.
        <*> pure Nothing
  where
    mProj = fst <$> mProjTags
    mTags = snd <$> mProjTags

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
viewForm = renderBootstrap3 BootstrapBasicForm $ liftA2 (,)
    (fmap (either (const defaultFilter) id . parseFilterExpression
                                           . fromMaybe "")
          (aopt' textField "filter" Nothing))
    (fmap (either (const defaultOrder) id . parseOrderExpression
                                          . fromMaybe "")
          (aopt' textField "sort" Nothing))

projectConfirmPledgeForm :: Maybe Int64 -> Form SharesPurchaseOrder
projectConfirmPledgeForm =
    renderBootstrap3 BootstrapBasicForm . fmap SharesPurchaseOrder
                                        . areq hiddenField ""
