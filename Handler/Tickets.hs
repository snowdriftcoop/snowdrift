{-# LANGUAGE TupleSections #-}
module Handler.Tickets where

import Import

import Widgets.Sidebar

import qualified Data.Map as M

import Data.Filter
import Data.Order
import Data.List (sort, sortBy)

import qualified Github.Issues as GH

import qualified Data.Set as S

import qualified Data.Text as T

import Yesod.Markdown (unMarkdown)

data AnnotatedTicket = AnnotatedTicket TicketId Ticket WikiPage Comment [Tag]

ticketToFilterable :: AnnotatedTicket -> Filterable
ticketToFilterable (AnnotatedTicket _ ticket _ comment tags) = Filterable has_tag get_named_ts search_literal
    where
        has_tag t = elem t $ map tagName tags
        get_named_ts "CREATED" = S.singleton $ ticketCreatedTs ticket
        get_named_ts name = error $ "Unrecognized time name " ++ T.unpack name
        search_literal str =
            (null $ T.breakOnAll str $ ticketName ticket)
            || (null . T.breakOnAll str $ unMarkdown $ commentText comment)

githubIssueToFilterable :: GH.Issue -> Filterable
githubIssueToFilterable i = Filterable has_tag get_named_ts search_literal
    where
        has_tag t = elem (T.unpack t) $ map GH.labelName $ GH.issueLabels i
        get_named_ts "CREATED" = S.singleton $ GH.fromGithubDate $ GH.issueCreatedAt i
        get_named_ts name = error $ "Unrecognized time name " ++ T.unpack name
        search_literal str =
            (null $ T.breakOnAll str $ T.pack $ GH.issueTitle i)
            || fromMaybe False (null . T.breakOnAll str . T.pack <$> GH.issueBody i)

ticketToOrderable :: AnnotatedTicket -> Orderable
ticketToOrderable (AnnotatedTicket _ ticket _ comment tags) = Orderable has_tag get_named_ts search_literal
    where
        has_tag t = elem t $ map tagName tags
        get_named_ts "CREATED" = S.singleton $ ticketCreatedTs ticket
        get_named_ts name = error $ "Unrecognized time name " ++ T.unpack name
        search_literal str =
            (null $ T.breakOnAll str $ ticketName ticket)
            || (null . T.breakOnAll str $ unMarkdown $ commentText comment)

githubIssueToOrderable :: GH.Issue -> Orderable
githubIssueToOrderable i = Orderable has_tag get_named_ts search_literal
    where
        has_tag t = elem (T.unpack t) $ map GH.labelName $ GH.issueLabels i
        get_named_ts "CREATED" = S.singleton $ GH.fromGithubDate $ GH.issueCreatedAt i
        get_named_ts name = error $ "Unrecognized time name " ++ T.unpack name
        search_literal str =
            (null $ T.breakOnAll str $ T.pack $ GH.issueTitle i)
            || fromMaybe False (null . T.breakOnAll str . T.pack <$> GH.issueBody i)

data Issue = Issue
    { issueWidget :: Widget
    , issueFilterable :: Filterable
    , issueOrderable :: Orderable
    }

    
defaultFilter :: Filterable -> Bool
defaultFilter = const True

defaultOrder :: Orderable -> [Double]
defaultOrder = const [0]

viewForm :: Form (Filterable -> Bool, Orderable -> [Double])
viewForm = renderDivs $ (,)
    <$> (either (const defaultFilter) id . parseFilterExpression . fromMaybe "" <$> aopt textField "filter" Nothing)
    <*> (either (const defaultOrder) id . parseOrderExpression . fromMaybe "" <$> aopt textField "sort" Nothing)


getTicketsR :: Text -> Handler Html
getTicketsR project_handle = do
    _ <- requireAuthId

    -- TODO: make this a project-specific (optional) setting
    github_issues' <- liftIO $ GH.issuesForRepo "dlthomas" (T.unpack project_handle) []

    github_issues <- case github_issues' of
        Right x -> return x
        Left _ -> setMessage "failed to fetch github tickets\n" >> return []

    ((result, formWidget), encType) <- runFormGet viewForm

    let (filter_expression, order_expression) = case result of
            FormSuccess x -> x
            _ -> (defaultFilter, defaultOrder)

    tickets :: [AnnotatedTicket] <- runDB $ do
        tickets'comments :: [(Entity Ticket, Entity Comment)] <- select $ from $ \ (comment `InnerJoin` ticket) -> do
            on_ $ comment ^. CommentId ==. ticket ^. TicketComment
            let pages = subList_select $ from $ \ (page `InnerJoin` project) -> do
                    on_ $ page ^. WikiPageProject ==. project ^. ProjectId
                    where_ $ project ^. ProjectHandle ==. val project_handle
                    return $ page ^. WikiPageId
             in where_ $ comment ^. CommentPage `in_` pages

            where_ $ comment ^. CommentId `notIn` subList_select (from $ \ retraction -> return $ retraction ^. CommentRetractionComment)
            return (ticket, comment)

        pages <- select $ from $ \ page -> do
            where_ $ page ^. WikiPageId `in_` valList (map (commentPage . entityVal . snd) tickets'comments)
            return page

        -- TODO Permissions: only show tickets on pages where I can view comments
        -- TODO Restrict pages to this project
            
        let pages_map = M.fromList . map (entityKey &&& entityVal) $ pages

        used_tags'tickets <- forM tickets'comments $ \ (Entity ticket_id ticket, Entity comment_id comment) -> do
            let page = pages_map M.! commentPage comment

            used_tags <- fmap (map $ \ (Value v) -> v) $ select $ from $ \ comment_tag -> do
                where_ $ comment_tag ^. CommentTagComment ==. val comment_id
                return $ comment_tag ^. CommentTagTag

            return $ (S.fromList used_tags, \ tags_map -> AnnotatedTicket ticket_id ticket page comment (map (tags_map M.!) used_tags))
            
        tags <- select $ from $ \ tag -> do
            where_ $ tag ^. TagId `in_` valList (S.toList $ mconcat $ map fst $ used_tags'tickets)
            return tag

        let tags_map = M.fromList . map (entityKey &&& entityVal) $ tags

        return $ map (($ tags_map) . snd) used_tags'tickets

    render <- getUrlRenderParams

    let ticketToIssue (AnnotatedTicket ticket_id ticket page comment tags) = Issue widget filterable orderable
                where
                    page_target = wikiPageTarget page
                    widget = [whamlet|
                            <tr>
                                <td>
                                    <a href="@{DiscussCommentR project_handle page_target (ticketComment ticket)}">
                                        SD-#{toPathPiece ticket_id}
                                <td>
                                    #{ticketName ticket}
                                <td>
                                    $forall tag <- sort $ map tagName tags
                                        <span .tag style="background-color:teal;font-size:xx-small">
                                            #{tag}
                        |]
                    filterable = ticketToFilterable $ AnnotatedTicket ticket_id ticket page comment tags
                    orderable = ticketToOrderable $ AnnotatedTicket ticket_id ticket page comment tags

        githubIssueToIssue github_issue = Issue widget filterable orderable
            where
                widget = [whamlet|
                        <tr>
                            <td>
                                $maybe url <- GH.issueHtmlUrl github_issue
                                    <a href="#{url}">
                                        GH-#{GH.issueNumber github_issue}
                                $nothing
                                    GH-#{GH.issueNumber github_issue}
                            <td>
                                #{GH.issueTitle github_issue}
                            <td>
                                $forall tag <- GH.issueLabels github_issue
                                    <span .tag style="background-color:##{GH.labelColor tag};font-size:xx-small">
                                        #{GH.labelName tag}
                                    
                    |]
                filterable = githubIssueToFilterable github_issue
                orderable = githubIssueToOrderable github_issue

        issues = sortBy (flip compare `on` order_expression . issueOrderable) $ filter (filter_expression . issueFilterable) $ map ticketToIssue tickets ++ map githubIssueToIssue github_issues

    defaultLayout $(widgetFile "tickets")
        
