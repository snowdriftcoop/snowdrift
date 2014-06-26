module View.Wiki where

import Import

import           Model.Comment       (makeModeratedComment)
import           Model.Permission
import           Model.Markdown
import           Model.User
import           Model.ViewType
import           Model.WikiPage
import           Widgets.Markdown
import           Widgets.Time
import           Widgets.Preview

import           Data.Algorithm.Diff (getDiff, Diff (..))
import qualified Data.Map            as M
import qualified Data.Set            as S
import qualified Data.Text           as T
import           Text.Blaze.Html5    (ins, del, br)
import           Yesod.Markdown

editWikiForm :: WikiEditId -> Markdown -> Maybe Text -> Form (WikiEditId, Markdown, Text)
editWikiForm last_edit_id content comment = renderBootstrap3 $ (,,)
        <$> areq' hiddenField "" (Just last_edit_id)
        <*> areq' snowdriftMarkdownField "Page Content" (Just content)
        <*> areq' textField "Comment" comment

editWikiPermissionsForm :: PermissionLevel -> Form PermissionLevel
editWikiPermissionsForm level = renderBootstrap3 $ areq permissionLevelField "Permission Level" (Just level)

newWikiForm :: Maybe Markdown -> Form Markdown
newWikiForm content = renderBootstrap3 $ areq' snowdriftMarkdownField "Page Content" content

renderWiki :: Int -> Text -> Text -> Bool -> Bool -> WikiPage -> Widget
renderWiki comment_count project_handle target can_edit can_view_meta page = $(widgetFile "wiki")
