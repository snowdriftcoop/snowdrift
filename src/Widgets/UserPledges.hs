
module Widgets.UserPledges where

import Import

import Model.Project
import qualified Mechanism as Mech

-- | A summary without ticket or discussion counts.
summarizeProject' :: Entity Project -> Mech.Project -> ProjectSummary
summarizeProject' a b = summarizeProject a b

-- |The summary of pledging to projects shown on user's page
userPledgeSummary :: UserId -> Widget
userPledgeSummary user_id = do
    project_summary <- handlerToWidget $ runDB $
        map (uncurry summarizeProject') <$> Mech.fetchUserPledgesDB user_id

    toWidget [hamlet|
        $if null project_summary
          not pledged to any projects
        $else
          <a href=@{UserPledgesR user_id}>
            <p>
              Patron to
              #{plural (length project_summary) "project" "projects"}
     |]

-- |The listing of all pledges for a given user, shown on u/#/pledges
userPledges :: UserId -> Widget
userPledges user_id = do
    project_summaries <- handlerToWidget $ runDB $
        map (uncurry summarizeProject') <$> Mech.fetchUserPledgesDB user_id

    toWidget [hamlet|
        $if null project_summaries
          not pledged to any projects
        $else
          <p>
            Note: For testing purposes only.  No real money is changing hands yet.
          <table .table>
            <tr>
              <th>Project
              <th>Pledge per patron
              <th>Patrons
              <th>Current monthly pledge value
            $forall summary <- project_summaries
              <tr>
                <td>
                  <a href=@{PHomeR (summaryProjectHandle summary)}>
                    #{summaryName summary}
                <td colspan=3><em>TODO: Mechanism data</em>
      |]
