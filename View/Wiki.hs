module View.Wiki where

import Import

import           Model.Permission
import           Model.Markdown
import           Widgets.Markdown

editWikiForm :: WikiEditId -> Markdown -> Maybe Text -> Form (WikiEditId, Markdown, Text)
editWikiForm last_edit_id content comment = renderBootstrap3 BootstrapBasicForm $ (,,)
    <$> areq' hiddenField "" (Just last_edit_id)
    <*> areq' snowdriftMarkdownField "Page Content" (Just content)
    <*> areq' textField "Comment" comment

editWikiPermissionsForm :: PermissionLevel -> Form PermissionLevel
editWikiPermissionsForm level = renderBootstrap3 BootstrapBasicForm $ areq permissionLevelField "Permission Level" (Just level)

newWikiForm :: Maybe Markdown -> Form Markdown
newWikiForm content = renderBootstrap3 BootstrapBasicForm $ areq' snowdriftMarkdownField "Page Content" content


newWikiTranslationForm :: Maybe WikiEditId -> Maybe Language -> Maybe Text -> Maybe Markdown -> Maybe Bool -> Form (WikiEditId, Language, Text, Markdown, Bool)
newWikiTranslationForm wiki_edit_id language target content complete = renderBootstrap3 BootstrapBasicForm $ (,,,,)
    <$> areq' hiddenField "" wiki_edit_id
    <*> areq' (selectField makeLanguageOptions) "New language" language
    <*> areq' textField "Target for URL" target
    <*> areq' snowdriftMarkdownField "Page content" content
    <*> areq  boolField "Complete translation" complete



renderWiki :: Int -> Text -> Language -> Text -> Bool -> [Language] -> WikiEdit -> Widget
renderWiki comment_count project_handle language target can_edit translations wiki_edit = do
    let wiki_target_id = key $ PersistInt64 (-1)
        wiki_target = WikiTarget
            (error "attempted to access page field of fake wiki target")
            (error "attempted to access project field of fake wiki target")
            target
            language

        discussion = DiscussionOnWikiPage (Entity wiki_target_id wiki_target)

    $(widgetFile "wiki")
