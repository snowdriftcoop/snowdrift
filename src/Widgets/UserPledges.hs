
module Widgets.UserPledges where

import Import

import Model.Project

-- | A summary without ticket or discussion counts.
summarizeProject' :: Entity Project -> ProjectSummary
summarizeProject' a = summarizeProject a
