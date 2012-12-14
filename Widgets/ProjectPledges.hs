
module Widgets.ProjectPledges where

import Import

import Model.Project

import Database.Persist.Query.Join.Sql (runJoin)
import Database.Persist.Query.Join (selectOneMany, SelectOneMany (..))


project_pledges :: UserId -> Widget
project_pledges user_id = do
    project_summaries <- lift $ runDB $ do
        projects <- runJoin $ (selectOneMany (PledgeProject <-.) pledgeProject) { somFilterMany = [ PledgeUser ==. user_id ] }
        mapM summarizeProject projects

    toWidget [hamlet|
        $if null project_summaries
            not contributing to any projects
        $else
            <p>
                note: this is for testing purposes only, no real money is changing hands yet
            <table .table>
                $forall summary <- project_summaries
                    <tr>
                        <td>
                            <a href="@{ProjectR (summaryProjectId summary)}">
                                #{summaryName summary}
                        <td>#{(show (summaryShareCost summary))}/share
                        <td>#{(show (getCount (summaryShares summary)))} shares
    |]
