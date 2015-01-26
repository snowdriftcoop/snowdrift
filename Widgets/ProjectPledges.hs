
module Widgets.ProjectPledges where

import Import

import Model.User (fetchUserProjectsPatronDB)
import Model.Project
import Model.Currency

{- projectPledgeSummary and projectPledges are most redundant with just
different widgets. We should probably just have one function and move
the widget stuff into two different hamlet files or something.
There are additional places that using the generalized function would
be useful, such as putting the summary number at /u listing perhaps-}

-- |The summary of pledging to projects shown on user's page
projectPledgeSummary :: UserId -> Widget
projectPledgeSummary user_id = do
    project_summary <- handlerToWidget $ runDB $ do
        projects_pledges <- fetchUserProjectsPatronDB user_id
--        projects_pledges <- fmap (map (second return)) $ select $ from $
--            \ (project `InnerJoin` pledge) -> do
--                on_ $ project ^. ProjectId ==. pledge ^. PledgeProject
--                where_ $ pledge ^. PledgeUser ==. val user_id
--                return (project, pledge)

        -- Discussion Counts & Ticket Counts not needed for this view
        mapM (\(a, b) -> summarizeProject a b [] []) projects_pledges

    toWidget [hamlet|
        $if null project_summary
            not pledged to any projects
        $else
            <a href="@{UserPledgesR user_id}">
                $if (length project_summary) == 1
                    <p>Patron to 1 project
                $else
                    <p>Patron to #{length project_summary} projects
     |]

-- |The listing of all pledges for a given user, shown on u/#/pledges
projectPledges :: UserId -> Widget
projectPledges user_id = do
    project_summaries :: [ProjectSummary] <- handlerToWidget $ runDB $ do
        projects_pledges <- fetchUserProjectsPatronDB user_id
--        projects_pledges <- fmap (map (second return)) $ select $ from $
--            \ (project `InnerJoin` pledge) -> do
--                on_ $ project ^. ProjectId ==. pledge ^. PledgeProject
--                where_ $ pledge ^. PledgeUser ==. val user_id
--                return (project, pledge)

        -- Discussion Counts & Ticket Counts not needed for this view
        mapM (\(a, b) -> summarizeProject a b [] []) projects_pledges

    let cost = summaryShareCost
        shares = getCount . summaryShares
        total x = cost x $* fromIntegral (shares x)

    toWidget [hamlet|
        $if null project_summaries
            not pledged to any projects
        $else
            <p>
                Note: For testing purposes only.  No real money is changing hands yet.
            <table .table>
                $forall summary <- project_summaries
                    <tr>
                        <td>
                            <a href="@{ProjectR (summaryProjectHandle summary)}">
                                #{summaryName summary}
                        <td>#{show (cost summary)}/share
                        <td>
                            $if (shares summary) == 1
                                #{show (shares summary)} share
                            $else
                                #{show (shares summary)} shares
                        <td>#{show (total summary)}
     |]

