module View.Wiki where

import Import

import           Model.Permission
import           Model.Markdown
import           Widgets.Markdown

editWikiForm :: WikiEditId -> Markdown -> Maybe Text -> Form (WikiEditId, Markdown, Text)
editWikiForm last_edit_id content comment = renderBootstrap3 $ (,,)
        <$> areq' hiddenField "" (Just last_edit_id)
        <*> areq' snowdriftMarkdownField "Page Content" (Just content)
        <*> areq' textField "Comment" comment

editWikiPermissionsForm :: PermissionLevel -> Form PermissionLevel
editWikiPermissionsForm level = renderBootstrap3 $ areq permissionLevelField "Permission Level" (Just level)

newWikiForm :: Maybe Markdown -> Form Markdown
newWikiForm content = renderBootstrap3 $ areq' snowdriftMarkdownField "Page Content" content

renderWiki :: Int -> Text -> Text -> Bool -> WikiPage -> Widget
renderWiki comment_count project_handle target can_edit page = $(widgetFile "wiki")
