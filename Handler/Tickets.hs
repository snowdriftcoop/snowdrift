{-# LANGUAGE TupleSections #-}

module Handler.Tickets where

import Import


import Widgets.Tag

import qualified Data.Map as M

import Data.Filter
import Data.Order
import Data.List (sortBy)

import qualified Github.Issues as GH

import qualified Data.Set as S

import qualified Data.Text as T

import Yesod.Markdown (unMarkdown)

import Model.AnnotatedTag

import Text.Printf
import Numeric

import Control.Concurrent.Async

data AnnotatedTicket = AnnotatedTicket TicketId Ticket WikiPage Comment [AnnotatedTag]

ticketToFilterable :: AnnotatedTicket -> Filterable
ticketToFilterable (AnnotatedTicket _ ticket _ comment tags) = Filterable has_tag get_named_ts search_literal
    where
        has_tag t = any (\ at -> atName at == t && atScore at > 0) tags
        get_named_ts "CREATED" = S.singleton $ ticketCreatedTs ticket
        get_named_ts "LAST UPDATED" = S.singleton $ ticketUpdatedTs ticket
        get_named_ts name = error $ "Unrecognized time name " ++ T.unpack name
        search_literal str =
            (null $ T.breakOnAll str $ ticketName ticket)
            || (null . T.breakOnAll str $ unMarkdown $ commentText comment)

githubIssueToFilterable :: GH.Issue -> Filterable
githubIssueToFilterable i = Filterable has_tag get_named_ts search_literal
    where
        has_tag t = elem (T.unpack t) $ map GH.labelName $ GH.issueLabels i
        get_named_ts "CREATED" = S.singleton $ GH.fromGithubDate $ GH.issueCreatedAt i
        get_named_ts "LAST UPDATED" = S.singleton $ GH.fromGithubDate $ GH.issueUpdatedAt i
        get_named_ts name = error $ "Unrecognized time name " ++ T.unpack name
        search_literal str =
            (null $ T.breakOnAll str $ T.pack $ GH.issueTitle i)
            || fromMaybe False (null . T.breakOnAll str . T.pack <$> GH.issueBody i)

ticketToOrderable :: AnnotatedTicket -> Orderable
ticketToOrderable (AnnotatedTicket _ ticket _ comment tags) = Orderable has_tag get_named_ts search_literal
    where
        has_tag t = elem t $ map atName tags
        get_named_ts "CREATED" = S.singleton $ ticketCreatedTs ticket
        get_named_ts "LAST UPDATED" = S.singleton $ ticketUpdatedTs ticket
        get_named_ts name = error $ "Unrecognized time name " ++ T.unpack name
        search_literal str =
            (null $ T.breakOnAll str $ ticketName ticket)
            || (null . T.breakOnAll str $ unMarkdown $ commentText comment)

githubIssueToOrderable :: GH.Issue -> Orderable
githubIssueToOrderable i = Orderable has_tag get_named_ts search_literal
    where
        has_tag t = elem (T.unpack t) $ map GH.labelName $ GH.issueLabels i
        get_named_ts "CREATED" = S.singleton $ GH.fromGithubDate $ GH.issueCreatedAt i
        get_named_ts "LAST UPDATED" = S.singleton $ GH.fromGithubDate $ GH.issueUpdatedAt i
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
viewForm = renderBootstrap3 $ (,)
    <$> (either (const defaultFilter) id . parseFilterExpression . fromMaybe "" <$> aopt' textField "filter" Nothing)
    <*> (either (const defaultOrder) id . parseOrderExpression . fromMaybe "" <$> aopt' textField "sort" Nothing)


getTicketsR :: Text -> Handler Html
getTicketsR project_handle = do
    _ <- requireAuthId


    Entity _ project <- runDB $ getBy404 $ UniqueProjectHandle project_handle

    get_github_issues <- liftIO $ async
        $ maybe (return (Right [])) (( \ (account, repo) -> GH.issuesForRepo account repo []) . second (drop 1) . break (== '/') . T.unpack)
        $ projectGithubRepo project


    ((result, formWidget), encType) <- runFormGet viewForm

    let (filter_expression, order_expression) = case result of
            FormSuccess x -> x
            _ -> (defaultFilter, defaultOrder)

    tickets :: [AnnotatedTicket] <- runDB $ do
        tickets'comments :: [(Entity Ticket, Entity Comment)] <- select $ from $ \ (comment `InnerJoin` ticket) -> do
            on_ $ comment ^. CommentId ==. ticket ^. TicketComment
            let pages = subList_select $ from $ \ (page `InnerJoin` proj) -> do
                    on_ $ page ^. WikiPageProject ==. proj ^. ProjectId
                    where_ $ proj ^. ProjectHandle ==. val project_handle
                    return $ page ^. WikiPageId
             in where_ $ comment ^. CommentPage `in_` pages

            where_ $ comment ^. CommentId `notIn` subList_select (from $ \ retraction -> return $ retraction ^. CommentRetractionComment)
            return (ticket, comment)

        pages <- select $ from $ \ page -> do
            where_ $ page ^. WikiPageId `in_` valList (map (commentPage . entityVal . snd) tickets'comments)
            return page

        let pages_map = M.fromList . map (entityKey &&& entityVal) $ pages

        used_tags'tickets <- forM tickets'comments $ \ (Entity ticket_id ticket, Entity comment_id comment) -> do
            let page = pages_map M.! commentPage comment

            used_tags <- select $ from $ \ comment_tag -> do
                where_ $ comment_tag ^. CommentTagComment ==. val comment_id
                return comment_tag

            let tags' :: [(TagId, (UserId, Int))]
                tags' = map ((commentTagTag &&& (commentTagUser &&& commentTagCount)) . entityVal) used_tags
                t tags_map = AnnotatedTicket ticket_id ticket page comment <$> buildAnnotatedTags tags_map (CommentTagR project_handle (wikiPageTarget page) comment_id) tags'

            return $ (S.fromList $ map (commentTagTag . entityVal) used_tags, t)

            
        tags <- select $ from $ \ tag -> do
            where_ $ tag ^. TagId `in_` valList (S.toList $ mconcat $ map fst $ used_tags'tickets)
            return tag

        let tags_map = M.fromList . map (entityKey &&& entityVal) $ tags

        mapM (\ (_, t) -> lift $ t tags_map) used_tags'tickets

    render <- getUrlRenderParams

    github_issues <- either (const $ addAlert "danger" "failed to fetch GitHub tickets\n" >> return []) return =<< liftIO (wait get_github_issues)

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
                                    $forall tag <- tags
                                        ^{tagWidget tag}
                        |]
                    filterable = ticketToFilterable $ AnnotatedTicket ticket_id ticket page comment tags
                    orderable = ticketToOrderable $ AnnotatedTicket ticket_id ticket page comment tags

        githubIssueToIssue github_issue = Issue widget filterable orderable
            where
                fg :: String -> String
                fg = printf "%06x" . pickForegroundColor . maybe 0 fst . listToMaybe . readHex
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
                                    <form .tag style="background-color:##{GH.labelColor tag};color:##{fg $ GH.labelColor tag};font-size:xx-small">
                                        #{GH.labelName tag}
                                    
                    |]
                filterable = githubIssueToFilterable github_issue
                orderable = githubIssueToOrderable github_issue


        issues = sortBy (flip compare `on` order_expression . issueOrderable) $ filter (filter_expression . issueFilterable) $ map ticketToIssue tickets ++ map githubIssueToIssue github_issues

    defaultLayout $(widgetFile "tickets")
        
