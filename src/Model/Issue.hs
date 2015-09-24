{-# LANGUAGE Rank2Types #-}

module Model.Issue where

import Import

import           Data.Filter
import           Data.Order
import           Widgets.Tag (pickForegroundColor)

import qualified Data.Set           as S
import qualified Data.Text          as T
import qualified Github.Issues      as GH
import           Numeric            (readHex)
import           Text.Printf

-- An Issue abstracts a Snowdrift ticket, Github issue, etc.
class Issue a where
    issueWidget     :: a -> Widget
    issueFilterable :: a -> Filterable
    issueOrderable  :: a -> Orderable

-- Existentially quantified Issue.
newtype SomeIssue =
    SomeIssue { unSomeIssue :: forall b. (forall a. Issue a => a -> b) -> b }

mkSomeIssue :: Issue a => a -> SomeIssue
mkSomeIssue issue = SomeIssue (\k -> k issue)

instance Issue SomeIssue where
    issueWidget     (SomeIssue k) = k issueWidget
    issueFilterable (SomeIssue k) = k issueFilterable
    issueOrderable  (SomeIssue k) = k issueOrderable

instance Issue GH.Issue where
    issueWidget github_issue =
        [whamlet|
          <tr>
            <td>
              $maybe url <- GH.issueHtmlUrl github_issue
                <a href=#{url}>
                  GH-#{GH.issueNumber github_issue}
              $nothing
                GH-#{GH.issueNumber github_issue}
            <td>
              #{GH.issueTitle github_issue}
            <td>
              $forall tag <- GH.issueLabels github_issue
                ^{githubIssueTagWidget tag}
        |]
      where
        githubIssueTagWidget :: GH.IssueLabel -> Widget
        githubIssueTagWidget tag = do
            [whamlet|
                <form .tag>
                  #{GH.labelName tag}
            |]
            toWidget [cassius|
              .tag
                background-color: ##{GH.labelColor tag}
                color: ##{fg $ GH.labelColor tag}
                font-size: xx-small
            |]

        fg :: String -> String
        fg = printf "%06x"
             . pickForegroundColor
             . maybe 0 fst
             . listToMaybe
             . readHex
    issueFilterable = mkFromGithubIssue Filterable
    issueOrderable = mkFromGithubIssue Orderable

mkFromGithubIssue
    :: ((Text -> Bool)
        -> (Text -> Bool)
        -> (Text -> Set UTCTime)
        -> (Text -> Bool)
        -> t)
    -> GH.Issue
    -> t
mkFromGithubIssue c i = c is_claimed has_tag get_named_ts search_literal
  where
    has_issue_assignee = isJust $ GH.issueAssignee i
    is_claimed "CLAIMED"   = has_issue_assignee
    -- | inverted in 'Data.Filter' and 'Data.Order'
    is_claimed "UNCLAIMED" = has_issue_assignee
    is_claimed cmd         = error $ "Unrecognized command " <> T.unpack cmd

    has_tag t = elem (T.unpack t) $ map GH.labelName $ GH.issueLabels i

    get_named_ts "CREATED" =
        S.singleton $ GH.fromGithubDate $ GH.issueCreatedAt i
    get_named_ts "LAST UPDATED" =
        S.singleton $ GH.fromGithubDate $ GH.issueUpdatedAt i
    get_named_ts name = error $ "Unrecognized time name " ++ T.unpack name

    search_literal str =
        not (null $ T.breakOnAll str $ T.pack $ GH.issueTitle i)
        || fromMaybe False (null . T.breakOnAll str . T.pack <$> GH.issueBody i)
