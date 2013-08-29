{-# LANGUAGE TupleSections #-}
module Handler.Tickets where

import Import

import Widgets.Sidebar

import qualified Data.Map as M
import Control.Arrow ((&&&))

import Data.Filter
import Data.Order
import Data.List (sort, sortBy)

import qualified Github.Issues as GH

import qualified Data.Set as S

import qualified Data.Text as T

import Yesod.Markdown (unMarkdown)

data AnnotatedTicket = AnnotatedTicket TicketId Ticket WikiPage Project Comment [Tag]

ticketToFilterable :: AnnotatedTicket -> Filterable
ticketToFilterable (AnnotatedTicket _ ticket _ _ comment tags) = Filterable has_tag get_named_ts search_literal
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
ticketToOrderable (AnnotatedTicket _ ticket _ _ comment tags) = Orderable has_tag get_named_ts search_literal
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

getTicketsR :: Text -> Handler Html
getTicketsR project_id = do
    Right github_issues <- liftIO $ GH.issuesForRepo "dlthomas" "snowdrift" [] -- TODO: make this a project-specific (optional) setting

    filter_expression <- either error id . parseFilterExpression . fromMaybe "" <$> lookupGetParam "filter"
    order_expression <- either error id . parseOrderExpression . fromMaybe "" <$> lookupGetParam "sort"

    tickets :: [AnnotatedTicket] <- runDB $ do
        tickets'comments :: [(Entity Ticket, Entity Comment)] <- select $ from $ \ (comment `InnerJoin` ticket) -> do
            on_ $ comment ^. CommentId ==. ticket ^. TicketComment
            where_ $ comment ^. CommentId `notIn` subList_select (from $ \ retraction -> return $ retraction ^. CommentRetractionComment)
            return (ticket, comment)

        pages <- select $ from $ \ page -> do
            where_ $ page ^. WikiPageId `in_` valList (map (commentPage . entityVal . snd) tickets'comments)
            return page

        projects :: [Entity Project] <- select $ from $ \ project -> do
            where_ $ project ^. ProjectId `in_` valList (map (wikiPageProject . entityVal) pages)
            return project

        -- TODO Permissions: only show tickets on pages where I can view comments
        -- TODO Restrict pages to this project
            
        let pages_map = M.fromList . map (entityKey &&& entityVal) $ pages
            projects_map :: Map ProjectId Project = M.fromList . map (entityKey &&& entityVal) $ projects

        used_tags'tickets <- forM tickets'comments $ \ (Entity ticket_id ticket, Entity comment_id comment) -> do
            let page = pages_map M.! commentPage comment
                project = projects_map M.! wikiPageProject page

            used_tags <- fmap (map $ \ (Value v) -> v) $ select $ from $ \ comment_tag -> do
                where_ $ comment_tag ^. CommentTagComment ==. val comment_id
                return $ comment_tag ^. CommentTagTag

            return $ (S.fromList used_tags, \ tags_map -> AnnotatedTicket ticket_id ticket page project comment (map (tags_map M.!) used_tags))
            
        tags <- select $ from $ \ tag -> do
            where_ $ tag ^. TagId `in_` valList (S.toList $ mconcat $ map fst $ used_tags'tickets)
            return tag

        let tags_map = M.fromList . map (entityKey &&& entityVal) $ tags

        return $ map (($ tags_map) . snd) used_tags'tickets

    render <- getUrlRenderParams

    let ticketToIssue (AnnotatedTicket ticket_id ticket page project comment tags) = Issue widget filterable orderable
                where
                    page_target = wikiPageTarget page
                    project_handle = projectHandle project
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
                    filterable = ticketToFilterable $ AnnotatedTicket ticket_id ticket page project comment tags
                    orderable = ticketToOrderable $ AnnotatedTicket ticket_id ticket page project comment tags

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

        issues = sortBy (compare `on` (order_expression . issueOrderable)) $ filter (filter_expression . issueFilterable) $ map ticketToIssue tickets ++ map githubIssueToIssue github_issues

    defaultLayout $(widgetFile "tickets")
        
