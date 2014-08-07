{-# LANGUAGE TupleSections, OverloadedStrings #-}

module Handler.Project where

import Import

import Model.Currency
import Model.Discussion
import Model.Project
import Model.Shares
import Model.Markdown
import Model.Markdown.Diff
import Model.SnowdriftEvent
import Model.User
import Model.WikiPage
import View.PledgeButton
import View.SnowdriftEvent
import Widgets.Markdown
import Widgets.Preview
import Widgets.Time

import           Data.List       (sortBy)
import qualified Data.Map        as M
import           Data.Maybe      (fromJust, maybeToList)
import qualified Data.Text       as T
import           Data.Time.Clock
import qualified Data.Set        as S
import           Yesod.Markdown

lookupGetParamDefault :: Read a => Text -> a -> Handler a
lookupGetParamDefault name def = do
    maybe_value <- lookupGetParam name
    return $ fromMaybe def $ maybe_value >>= readMaybe . T.unpack

getProjectsR :: Handler Html
getProjectsR = do
    projects <- runDB fetchAllProjectsDB

    defaultLayout $ do
        setTitle "Projects | Snowdrift.coop"
        $(widgetFile "projects")

getProjectPledgeButtonR :: Text -> Handler TypedContent
getProjectPledgeButtonR project_handle = do
   pledges <- runYDB $ do
        Entity project_id _project <- getBy404 $ UniqueProjectHandle project_handle
        getProjectShares project_id
   let png = overlayImage blankPledgeButton $
        fillInPledgeCount (fromIntegral (length pledges))
   respond "image/png" png

getProjectR :: Text -> Handler Html
getProjectR project_handle = do
    maybe_viewer_id <- maybeAuthId

    (project_id, project, pledges, pledge) <- runYDB $ do
        Entity project_id project <- getBy404 $ UniqueProjectHandle project_handle
        pledges <- getProjectShares project_id
        pledge <- case maybe_viewer_id of
            Nothing -> return Nothing
            Just viewer_id -> getBy $ UniquePledge viewer_id project_id

        return (project_id, project, pledges, pledge)

    defaultLayout $ do
        setTitle . toHtml $ projectName project <> " | Snowdrift.coop"
        renderProject (Just project_id) project pledges pledge


renderProject :: Maybe ProjectId
              -> Project
              -> [Int64]
              -> Maybe (Entity Pledge)
              -> WidgetT App IO ()
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


data UpdateProject = UpdateProject { updateProjectName :: Text, updateProjectDescription :: Markdown, updateProjectTags :: [Text], updateProjectGithubRepo :: Maybe Text } deriving Show


editProjectForm :: Maybe (Project, [Text]) -> Form UpdateProject
editProjectForm project =
    renderBootstrap3 $ UpdateProject
        <$> areq' textField "Project Name" (projectName . fst <$> project)
        <*> areq' snowdriftMarkdownField "Description" (projectDescription . fst <$> project)
        <*> (maybe [] (map T.strip . T.splitOn ",") <$> aopt' textField "Tags" (Just . T.intercalate ", " . snd <$> project))
        <*> aopt' textField "Github Repository" (projectGithubRepo . fst <$> project)


getEditProjectR :: Text -> Handler Html
getEditProjectR project_handle = do
    viewer_id <- requireAuthId

    Entity project_id project <- runYDB $ do
        can_edit <- (||) <$> isProjectAdmin project_handle viewer_id <*> isProjectAdmin "snowdrift" viewer_id
        if can_edit
         then getBy404 $ UniqueProjectHandle project_handle
         else permissionDenied "You do not have permission to edit this project."

    tags <- runDB $
        select $
        from $ \ (p_t `InnerJoin` tag) -> do
        on_ (p_t ^. ProjectTagTag ==. tag ^. TagId)
        where_ (p_t ^. ProjectTagProject ==. val project_id)
        return tag

    (project_form, _) <- generateFormPost $ editProjectForm (Just (project, map (tagName . entityVal) tags))

    defaultLayout $ do
        setTitle . toHtml $ projectName project <> " | Snowdrift.coop"
        $(widgetFile "edit_project")


postProjectR :: Text -> Handler Html
postProjectR project_handle = do
    viewer_id <- requireAuthId

    Entity project_id project <- runYDB $ do
        can_edit <- (||) <$> isProjectAdmin project_handle viewer_id <*> isProjectAdmin "snowdrift" viewer_id
        if can_edit
         then getBy404 $ UniqueProjectHandle project_handle
         else permissionDenied "You do not have permission to edit this project."

    ((result, _), _) <- runFormPost $ editProjectForm Nothing

    now <- liftIO getCurrentTime

    case result of
        FormSuccess (UpdateProject name description tags github_repo) -> do
            mode <- lookupPostParam "mode"
            let action :: Text = "update"
            case mode of
                Just "preview" -> do
                    let preview_project = project { projectName = name, projectDescription = description, projectGithubRepo = github_repo }

                    (form, _) <- generateFormPost $ editProjectForm (Just (preview_project, tags))
                    defaultLayout $ previewWidget form action $ renderProject (Just project_id) preview_project [] Nothing

                Just x | x == action -> do
                    runDB $ do
                        when (projectDescription project /= description) $ do
                            project_update <- insert $ ProjectUpdate now project_id viewer_id $ diffMarkdown (projectDescription project) description
                            last_update <- getBy $ UniqueProjectLastUpdate project_id
                            case last_update of
                                Just (Entity key _) -> repsert key $ ProjectLastUpdate project_id project_update
                                Nothing -> void $ insert $ ProjectLastUpdate project_id project_update

                        update $ \ p -> do
                            set p [ ProjectName =. val name, ProjectDescription =. val description, ProjectGithubRepo =. val github_repo ]
                            where_ (p ^. ProjectId ==. val project_id)

                        tag_ids <- forM tags $ \ tag_name -> do
                            tag_entity_list <- select $ from $ \ tag -> do
                                where_ (tag ^. TagName ==. val tag_name)
                                return tag

                            case tag_entity_list of
                                [] -> insert $ Tag tag_name
                                Entity tag_id _ : _ -> return tag_id


                        delete $ from $ \ project_tag -> where_ (project_tag ^. ProjectTagProject ==. val project_id)

                        forM_ tag_ids $ \ tag_id -> insert $ ProjectTag project_id tag_id

                    addAlert "success" "project updated"
                    redirect $ ProjectR project_handle

                _ -> do
                    addAlertEm "danger" "unrecognized mode" "Error: "
                    redirect $ ProjectR project_handle
        x -> do
            addAlert "danger" $ T.pack $ show x
            redirect $ ProjectR project_handle


getProjectPatronsR :: Text -> Handler Html
getProjectPatronsR project_handle = do
    _ <- requireAuthId

    page <- lookupGetParamDefault "page" 0
    per_page <- lookupGetParamDefault "count" 20

    (project, pledges, user_payouts_map) <- runYDB $ do
        Entity project_id project <- getBy404 $ UniqueProjectHandle project_handle
        pledges <- select $ from $ \ (pledge `InnerJoin` user) -> do
            on_ $ pledge ^. PledgeUser ==. user ^. UserId
            where_ $ pledge ^. PledgeProject ==. val project_id
                &&. pledge ^. PledgeFundedShares >. val 0
            orderBy [ desc (pledge ^. PledgeFundedShares), asc (user ^. UserName), asc (user ^. UserId)]
            offset page
            limit per_page
            return (pledge, user)

        last_paydays <- case projectLastPayday project of
            Nothing -> return []
            Just last_payday -> select $ from $ \ payday -> do
                where_ $ payday ^. PaydayId <=. val last_payday
                orderBy [ desc $ payday ^. PaydayId ]
                limit 2
                return payday

        user_payouts <- select $ from $ \ (transaction `InnerJoin` user) -> do
            where_ $ transaction ^. TransactionPayday `in_` valList (map (Just . entityKey) last_paydays)
            on_ $ transaction ^. TransactionDebit ==. just (user ^. UserAccount)
            groupBy $ user ^. UserId
            return (user ^. UserId, count $ transaction ^. TransactionId)

        return (project, pledges, M.fromList $ map ((\ (Value x :: Value UserId) -> x) *** (\ (Value x :: Value Int) -> x)) user_payouts)

    defaultLayout $ do
        setTitle . toHtml $ projectName project <> " Patrons | Snowdrift.coop"
        $(widgetFile "project_patrons")

getProjectTransactionsR :: Text -> Handler Html
getProjectTransactionsR project_handle = do
    (project, account, account_map, transaction_groups) <- runYDB $ do
        Entity _ project :: Entity Project <- getBy404 $ UniqueProjectHandle project_handle

        account <- get404 $ projectAccount project

        transactions <- select $ from $ \ t -> do
            where_ $ t ^. TransactionCredit ==. val (Just $ projectAccount project)
                    ||. t ^. TransactionDebit ==. val (Just $ projectAccount project)

            orderBy [ desc $ t ^. TransactionTs ]
            return t

        let accounts = S.toList $ S.fromList $ concatMap (\ (Entity _ t) -> maybeToList (transactionCredit t) <> maybeToList (transactionDebit t)) transactions

        users_by_account <- fmap (M.fromList . map (userAccount . entityVal &&& Right)) $ select $ from $ \ u -> do
            where_ $ u ^. UserAccount `in_` valList accounts
            return u

        projects_by_account <- fmap (M.fromList . map (projectAccount . entityVal &&& Left)) $ select $ from $ \ p -> do
            where_ $ p ^. ProjectAccount `in_` valList accounts
            return p

        let account_map = projects_by_account `M.union` users_by_account

        payday_map <- fmap (M.fromList . map (entityKey &&& id)) $ select $ from $ \ pd -> do
            where_ $ pd ^. PaydayId `in_` valList (S.toList $ S.fromList $ mapMaybe (transactionPayday . entityVal) transactions)
            return pd

        return (project, account, account_map, process payday_map transactions)

    let getOtherAccount transaction
            | transactionCredit transaction == Just (projectAccount project) = transactionDebit transaction
            | transactionDebit transaction == Just (projectAccount project) = transactionCredit transaction
            | otherwise = Nothing

    defaultLayout $ do
        setTitle . toHtml $ projectName project <> " Transactions | Snowdrift.coop"
        $(widgetFile "project_transactions")

  where
    process payday_map =
        let process' [] [] = []
            process' (t':ts') [] = [(fmap (payday_map M.!) $ transactionPayday $ entityVal t', reverse (t':ts'))]
            process' [] (t:ts) = process' [t] ts

            process' (t':ts') (t:ts)
                | transactionPayday (entityVal t') == transactionPayday (entityVal t)
                = process' (t:t':ts') ts
                | otherwise
                = (fmap (payday_map M.!) $ transactionPayday $ entityVal t', reverse (t':ts')) : process' [t] ts
         in process' []


getProjectBlogR :: Text -> Handler Html
getProjectBlogR project_handle = do
    maybe_from <- fmap (Key . PersistInt64 . read . T.unpack) <$> lookupGetParam "from"
    post_count <- fromMaybe 10 <$> fmap (read . T.unpack) <$> lookupGetParam "from"
    Entity project_id project <- runYDB $ getBy404 $ UniqueProjectHandle project_handle

    let apply_offset blog = maybe id (\ from_blog rest -> blog ^. ProjectBlogId >=. val from_blog &&. rest) maybe_from

    (posts, next) <- fmap (splitAt post_count) $ runDB $
        select $
        from $ \blog -> do
        where_ $ apply_offset blog $ blog ^. ProjectBlogProject ==. val project_id
        orderBy [ desc $ blog ^. ProjectBlogTime, desc $ blog ^. ProjectBlogId ]
        limit (fromIntegral post_count + 1)
        return blog

    renderRouteParams <- getUrlRenderParams

    let nextRoute next_id = renderRouteParams (ProjectBlogR project_handle) [("from", toPathPiece next_id)]

    defaultLayout $ do
        setTitle . toHtml $ projectName project <> " Blog | Snowdrift.coop"
        $(widgetFile "project_blog")

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


postProjectBlogR :: Text -> Handler Html
postProjectBlogR project_handle = do
    viewer_id <- requireAuthId

    Entity project_id _ <- runYDB $ do
        can_edit <- or <$> sequence
            [ isProjectAdmin project_handle viewer_id
            , isProjectTeamMember project_handle viewer_id
            , isProjectAdmin "snowdrift" viewer_id
            ]

        if can_edit
         then getBy404 $ UniqueProjectHandle project_handle
         else permissionDenied "You do not have permission to edit this project."

    now <- liftIO getCurrentTime

    ((result, _), _) <- runFormPost $ projectBlogForm now viewer_id project_id

    case result of
        FormSuccess blog_post' -> do
            let blog_post :: ProjectBlog
                blog_post = blog_post' { projectBlogTime = now, projectBlogUser = viewer_id }
            mode <- lookupPostParam "mode"
            let action :: Text = "post"
            case mode of
                Just "preview" -> do

                    (form, _) <- generateFormPost $ projectBlogForm now viewer_id project_id

                    defaultLayout $ previewWidget form action $ renderBlogPost project_handle blog_post

                Just x | x == action -> do
                    void $ runDB $ insert blog_post
                    addAlert "success" "posted"
                    redirect $ ProjectR project_handle

                _ -> do
                    addAlertEm "danger" "unrecognized mode" "Error: "
                    redirect $ ProjectR project_handle

        x -> do
            addAlert "danger" $ T.pack $ show x
            redirect $ ProjectR project_handle


getProjectBlogPostR :: Text -> ProjectBlogId -> Handler Html
getProjectBlogPostR project_handle blog_post_id = do
    (Entity _ project, blog_post) <- runYDB $ (,)
        <$> getBy404 (UniqueProjectHandle project_handle)
        <*> get404 blog_post_id

    defaultLayout $ do
        setTitle . toHtml $ projectName project <> " Blog - " <> projectBlogTitle blog_post <> " | Snowdrift.coop"
        renderBlogPost project_handle blog_post


renderBlogPost :: Text -> ProjectBlog -> WidgetT App IO ()
renderBlogPost project_handle blog_post = do
    let (Markdown top_content) = projectBlogTopContent blog_post
        (Markdown bottom_content) = fromMaybe (Markdown "") $ projectBlogBottomContent blog_post
        title = projectBlogTitle blog_post
        content = markdownWidget project_handle $ Markdown $ T.snoc top_content '\n' <> bottom_content

    $(widgetFile "blog_post")

postWatchProjectR, postUnwatchProjectR :: ProjectId -> Handler ()
postWatchProjectR   = watchOrUnwatchProject userWatchProjectDB   "watching "
postUnwatchProjectR = watchOrUnwatchProject userUnwatchProjectDB "no longer watching "

watchOrUnwatchProject :: (UserId -> ProjectId -> DB ()) -> Text -> ProjectId -> Handler ()
watchOrUnwatchProject action msg project_id = do
    user_id <- requireAuthId
    project <- runYDB $ do
        action user_id project_id
        get404 project_id
    addAlert "success" (msg <> projectName project)
    redirect HomeR

--------------------------------------------------------------------------------
-- /feed

-- | This function is responsible for hitting every relevant event table. Nothing
-- statically guarantees that.
getProjectFeedR :: Text -> Handler Html
getProjectFeedR project_handle = do
    muser_id <- maybeAuthId
    before <- maybe (liftIO getCurrentTime) (return . read . T.unpack) =<< lookupGetParam "before"
    (events, discussion_wiki_pages_map, wiki_pages_map, _) <- runYDB $ do
        Entity project_id _ <- getBy404 (UniqueProjectHandle project_handle)

        comment_posted_entities  <- fetchProjectCommentsPostedOnWikiPagesBeforeDB project_id muser_id before
        comment_pending_entities <- fetchProjectCommentsPendingBeforeDB project_id muser_id before
        wiki_page_entities       <- fetchProjectWikiPagesBeforeDB project_id before
        wiki_edit_entities       <- fetchProjectWikiEditsBeforeDB project_id before
        new_pledge_entities      <- fetchProjectNewPledgesBeforeDB project_id before
        updated_pledges          <- fetchProjectUpdatedPledgesBeforeDB project_id before
        deleted_pledge_events    <- fetchProjectDeletedPledgesBeforeDB project_id before

        -- Suplementary maps for displaying the data. If something above requires extra
        -- data to display the project feed row, it MUST be used to fetch the data below!
        -- The Maybes from Data.Map.lookup are unsafely STRIPPED in the views!

        let comments   = map entityVal (comment_posted_entities <> comment_pending_entities)
            wiki_edits = map entityVal wiki_edit_entities
            pledges    = map entityVal (new_pledge_entities <> (map snd updated_pledges))

        -- WikiPages that can be keyed by a Comment's DiscussionId (i.e. the Comment *is* on a WikiPage)
        discussion_wiki_pages_map <- M.fromList . map (\e@(Entity _ WikiPage{..}) -> (wikiPageDiscussion, e)) <$>
                                       fetchDiscussionWikiPagesInDB (map commentDiscussion comments)

        -- WikiPages keyed by their own IDs (contained in a WikiEdit)
        wiki_pages_map <- entitiesMap <$> fetchWikiPagesInDB (map wikiEditPage wiki_edits)

        -- All users: Comment posters, WikiPage creators, WikiEdit makers,
        -- and Pledgers (new, updated, and deleted).
        users_map <- (\a b c d -> a <> b <> c <> d)
            <$> (entitiesMap <$> fetchUsersInDB (map commentUser comments))
            <*> (entitiesMap <$> fetchUsersInDB (map wikiEditUser wiki_edits))
            <*> (entitiesMap <$> fetchUsersInDB (map pledgeUser pledges))
            <*> (entitiesMap <$> fetchUsersInDB (map (\(EventDeletedPledge _ user_id _ _) -> user_id) deleted_pledge_events))

        let events = sortBy snowdriftEventNewestToOldest . mconcat $
              [ map (onEntity ECommentPosted)  comment_posted_entities
              , map (onEntity ECommentPending) comment_pending_entities
              , map (onEntity EWikiPage)       wiki_page_entities
              , map (onEntity EWikiEdit)       wiki_edit_entities
              , map (onEntity ENewPledge)      new_pledge_entities
              , map eup2se                     updated_pledges
              , map edp2se                     deleted_pledge_events
              ]
        return (events, discussion_wiki_pages_map, wiki_pages_map, users_map)
    defaultLayout $(widgetFile "project_feed")
  where
    -- "event updated pledge to snowdrift event". Makes above code cleaner.
    eup2se :: (Int64, Entity Pledge) -> SnowdriftEvent
    eup2se (old_shares, Entity pledge_id pledge) = EUpdatedPledge old_shares pledge_id pledge

    -- "event deleted pledge to snowdrift event". Makes above code cleaner.
    edp2se :: EventDeletedPledge -> SnowdriftEvent
    edp2se (EventDeletedPledge a b c d) = EDeletedPledge a b c d
