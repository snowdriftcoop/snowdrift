{-# LANGUAGE Rank2Types #-}

module Model.Issue where

import Import

import           Data.Filter
import           Data.Order
import           Model.AnnotatedTag
import           Model.Ticket
import           Widgets.Tag        (pickForegroundColor, tagWidget)

import qualified Data.Set           as S
import qualified Data.Text          as T
import qualified Github.Issues      as GH
import           Numeric            (readHex)
import           Text.Printf
import           Yesod.Markdown     (unMarkdown)

-- An Issue abstracts a Snowdrift ticket, Github issue, etc.
class Issue a where
    issueWidget     :: a -> Widget
    issueFilterable :: a -> Filterable
    issueOrderable  :: a -> Orderable

-- Existentially quantified Issue.
newtype SomeIssue = SomeIssue { unSomeIssue :: forall b. (forall a. Issue a => a -> b) -> b }

mkSomeIssue :: Issue a => a -> SomeIssue
mkSomeIssue issue = SomeIssue (\k -> k issue)

instance Issue SomeIssue where
    issueWidget     (SomeIssue k) = k issueWidget
    issueFilterable (SomeIssue k) = k issueFilterable
    issueOrderable  (SomeIssue k) = k issueOrderable

instance Issue AnnotatedTicket where
    issueWidget (AnnotatedTicket project_handle ticket_id ticket page comment tags) =
        [whamlet|
          <tr>
            <td>
              <a href="@{DiscussCommentR project_handle (wikiPageTarget page) (ticketComment ticket)}">
                SD-#{toPathPiece ticket_id}
            <td>
              #{ticketName ticket}
            <td>
              $forall tag <- tags
                ^{tagWidget tag}
        |]
    issueFilterable = ticketToFilterable
    issueOrderable = ticketToOrderable

ticketToFilterable :: AnnotatedTicket -> Filterable
ticketToFilterable (AnnotatedTicket _ _ ticket _ comment tags) = Filterable has_tag get_named_ts search_literal
  where
    has_tag t = any (\ at -> atName at == t && atScore at > 0) tags

    get_named_ts "CREATED" = S.singleton $ ticketCreatedTs ticket
    get_named_ts "LAST UPDATED" = S.singleton $ ticketUpdatedTs ticket
    get_named_ts name = error $ "Unrecognized time name " ++ T.unpack name

    search_literal str = uncurry ((||) `on` not . null . T.breakOnAll str) (ticketName ticket, unMarkdown $ commentText comment)

ticketToOrderable :: AnnotatedTicket -> Orderable
ticketToOrderable (AnnotatedTicket _ _ ticket _ comment tags) = Orderable has_tag get_named_ts search_literal
  where
    has_tag t = elem t $ map atName tags
    get_named_ts "CREATED" = S.singleton $ ticketCreatedTs ticket
    get_named_ts "LAST UPDATED" = S.singleton $ ticketUpdatedTs ticket
    get_named_ts name = error $ "Unrecognized time name " ++ T.unpack name
    search_literal str = uncurry ((||) `on` not . null . T.breakOnAll str) (ticketName ticket, unMarkdown $ commentText comment)

instance Issue GH.Issue where
    issueWidget github_issue =
        [whamlet|
          <tr>
            <td>
              $maybe url <- GH.issueHtmlUrl github_issue
                <a href="#{url}">
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
        fg = printf "%06x" . pickForegroundColor . maybe 0 fst . listToMaybe . readHex
    issueFilterable = mkFromGithubIssue Filterable
    issueOrderable = mkFromGithubIssue Orderable

mkFromGithubIssue :: ((Text -> Bool) -> (Text -> Set UTCTime) -> (Text -> Bool) -> t) -> GH.Issue -> t
mkFromGithubIssue c i = c has_tag get_named_ts search_literal
  where
    has_tag t = elem (T.unpack t) $ map GH.labelName $ GH.issueLabels i

    get_named_ts "CREATED" = S.singleton $ GH.fromGithubDate $ GH.issueCreatedAt i
    get_named_ts "LAST UPDATED" = S.singleton $ GH.fromGithubDate $ GH.issueUpdatedAt i
    get_named_ts name = error $ "Unrecognized time name " ++ T.unpack name

    search_literal str =
            not (null $ T.breakOnAll str $ T.pack $ GH.issueTitle i)
                    || fromMaybe False (null . T.breakOnAll str . T.pack <$> GH.issueBody i)
