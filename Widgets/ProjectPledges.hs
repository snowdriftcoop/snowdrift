
module Widgets.ProjectPledges where

import Import

import Model.Project
import Model.Currency

project_pledges :: UserId -> Widget
project_pledges user_id = do
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
                note: for testing purposes only, no real money is changing hands yet
            <table .table>
                $forall summary <- project_summaries
                    <tr>
                        <td>
                            <a href="@{ProjectR (summaryProjectHandle summary)}">
                                #{summaryName summary}
                        <td>#{show (cost summary)}/share
                        <td>#{show (shares summary)}&nbsp;shares
                        <td>#{show (total summary)}
    |]
