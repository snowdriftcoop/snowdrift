
module Widgets.ProjectPledges where

import Import

import Model.Currency
import Model.Count
import Model.Project
import Model.User (fetchUserPledgesDB)

-- | A summary without ticket or discussion counts.
summarizeProject' :: Entity Project -> [Entity Pledge] -> ProjectSummary
summarizeProject' a b = summarizeProject a b [] []

-- |The summary of pledging to projects shown on user's page
projectPledgeSummary :: UserId -> Widget
projectPledgeSummary user_id = do
    project_summary <- handlerToWidget $ runDB $
        map (uncurry summarizeProject') <$> fetchUserPledgesDB user_id

    toWidget [hamlet|
        $if null project_summary
          not pledged to any projects
        $else
          <a href=@{UserPledgesR user_id}>
            <p>Patron to #{plural (length project_summary) "project" "projects"}
     |]

-- |The listing of all pledges for a given user, shown on u/#/pledges
projectPledges :: UserId -> Widget
projectPledges user_id = do
    project_summaries <- handlerToWidget $ runDB $
        map (uncurry summarizeProject') <$> fetchUserPledgesDB user_id

    let cost = summaryShareCost
        shares = getCount . summaryShares
        mills = millMilray . shares
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
                            <a href=@{ProjectR (summaryProjectHandle summary)}>
                                #{summaryName summary}
                        <td>#{show (cost summary)}/pledge
                        <td>#{show (mills summary)}
                        <td>#{show (total summary)}
     |]

