
module Widgets.ProjectPledges where

import Import

import Model.Project
import Model.Currency

projectPledgeSummary :: UserId -> Widget
projectPledgeSummary user_id = do
    project_summary :: [ProjectSummary] <- handlerToWidget $ runDB $ do
        projects_pledges <- fmap (map (second return)) $ select $ from $ \ (project `InnerJoin` pledge) -> do
            on_ $ project ^. ProjectId ==. pledge ^. PledgeProject
            where_ $ pledge ^. PledgeUser ==. val user_id
            return (project, pledge)

        mapM (uncurry summarizeProject) projects_pledges

    toWidget [hamlet|
        $if null project_summary
            not supporting any projects
        $else
            <a href="@{UserPledgesR user_id}">
                $if (length project_summary) == 1
                    <p>Patron to 1 project
                $else
                    <p>Patron to (length project_summary) projects
     |]


projectPledges :: UserId -> Widget
projectPledges user_id = do
    project_summaries :: [ProjectSummary] <- handlerToWidget $ runDB $ do
        projects_pledges <- fmap (map (second return)) $ select $ from $ \ (project `InnerJoin` pledge) -> do
            on_ $ project ^. ProjectId ==. pledge ^. PledgeProject
            where_ $ pledge ^. PledgeUser ==. val user_id
            return (project, pledge)

        mapM (uncurry summarizeProject) projects_pledges

    let cost = summaryShareCost
        shares = getCount . summaryShares
        total x = cost x $* fromIntegral (shares x)

    toWidget [hamlet|
        $if null project_summaries
            not supporting any projects
        $else
            <p>
                Note: For testing purposes only.  No real money is changing hands yet
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
