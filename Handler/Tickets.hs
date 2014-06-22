{-# LANGUAGE TupleSections #-}

module Handler.Tickets where

import Import

import           Data.Filter
import           Data.Order
import           Model.Issue
import           Model.Project            (getGithubIssues)
import           Model.Ticket             (getTickets)

import           Data.List                (sortBy)

viewForm :: Form (Filterable -> Bool, Orderable -> [Double])
viewForm = renderBootstrap3 $ (,)
    <$> (either (const defaultFilter) id . parseFilterExpression . fromMaybe "" <$> aopt' textField "filter" Nothing)
    <*> (either (const defaultOrder) id . parseOrderExpression . fromMaybe "" <$> aopt' textField "sort" Nothing)


getTicketsR :: Text -> Handler Html
getTicketsR project_handle = do
    Entity project_id project <- runDB $ getBy404 $ UniqueProjectHandle project_handle

    ((result, formWidget), encType) <- runFormGet viewForm

    let (filter_expression, order_expression) = case result of
            FormSuccess x -> x
            _ -> (defaultFilter, defaultOrder)

    tickets       <- runDB $ getTickets project_id project_handle
    github_issues <- getGithubIssues project

    let issues = sortBy (flip compare `on` order_expression . issueOrderable) $
                   filter (filter_expression . issueFilterable) $
                      map mkSomeIssue tickets ++ map mkSomeIssue github_issues

    defaultLayout $ do
        setTitle . toHtml $ projectName project <> " Tickets | Snowdrift.coop"
        $(widgetFile "tickets")
