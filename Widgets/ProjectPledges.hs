
module Widgets.ProjectPledges where

import Import

import Model.User (fetchUserProjectsPatronDB)
import Model.Project
import Model.Currency

-- | A summary without ticket or discussion counts.
summarizeProject' :: Entity Project -> [Entity Pledge] -> ProjectSummary
summarizeProject' a b = summarizeProject a b [] []

-- |The summary of pledging to projects shown on user's page
projectPledgeSummary :: UserId -> Widget
projectPledgeSummary user_id = do
    project_summary <- handlerToWidget $ runDB $
        map (uncurry summarizeProject') <$> fetchUserProjectsPatronDB user_id

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
    project_summaries <- handlerToWidget $ runDB $
        map (uncurry summarizeProject') <$> fetchUserProjectsPatronDB user_id

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

