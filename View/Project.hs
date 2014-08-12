module View.Project where

import Import

import           Data.Filter
import           Data.Order
import           Model.Currency
import           Model.Markdown
import           Model.Project
import           Model.Shares
import           Model.Role
import           Widgets.Markdown

import qualified Data.Text       as T
import           Data.Time.Clock
import           Yesod.Markdown

renderProject :: Maybe ProjectId -> Project -> [Int64] -> Maybe (Entity Pledge) -> WidgetT App IO ()
renderProject maybe_project_id project pledges pledge = do
    let share_value = projectShareValue project
        users = fromIntegral $ length pledges
        shares = sum pledges
        project_value = share_value $* fromIntegral shares
        description = markdownWidget (projectHandle project) $ projectDescription project

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

renderBlogPost :: Text -> ProjectBlog -> WidgetT App IO ()
renderBlogPost project_handle blog_post = do
    let (Markdown top_content) = projectBlogTopContent blog_post
        (Markdown bottom_content) = fromMaybe (Markdown "") $ projectBlogBottomContent blog_post
        title = projectBlogTitle blog_post
        content = markdownWidget project_handle $ Markdown $ T.snoc top_content '\n' <> bottom_content
    $(widgetFile "blog_post")

editProjectForm :: Maybe (Project, [Text]) -> Form UpdateProject
editProjectForm project =
    renderBootstrap3 $ UpdateProject
        <$> areq' textField "Project Name" (projectName . fst <$> project)
        <*> areq' snowdriftMarkdownField "Description" (projectDescription . fst <$> project)
        <*> (maybe [] (map T.strip . T.splitOn ",") <$> aopt' textField "Tags" (Just . T.intercalate ", " . snd <$> project))
        <*> aopt' textField "Github Repository" (projectGithubRepo . fst <$> project)

projectBlogForm :: UTCTime -> UserId -> ProjectId -> Form ProjectBlog
projectBlogForm now user_id project_id =
    renderBootstrap3 $ mkBlog
        <$> areq' textField "Post Title" Nothing
        <*> areq' snowdriftMarkdownField "Post" Nothing
  where
    mkBlog :: Text -> Markdown -> ProjectBlog
    mkBlog title (Markdown content) =
        let (top_content, bottom_content) = break (== "---") $ T.lines content
         in ProjectBlog now title user_id project_id undefined (Markdown $ T.unlines top_content) (if null bottom_content then Nothing else Just $ Markdown $ T.unlines bottom_content)

inviteForm :: Form (Text, Role)
inviteForm = renderBootstrap3 $ (,)
    <$> areq' textField "About this invitation:" Nothing
    <*> areq roleField "Type of Invite:" (Just TeamMember)

viewForm :: Form (Filterable -> Bool, Orderable -> [Double])
viewForm = renderBootstrap3 $ (,)
    <$> (either (const defaultFilter) id . parseFilterExpression . fromMaybe "" <$> aopt' textField "filter" Nothing)
    <*> (either (const defaultOrder) id . parseOrderExpression . fromMaybe "" <$> aopt' textField "sort" Nothing)
