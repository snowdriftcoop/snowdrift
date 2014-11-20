module View.Wiki where

import Import

import           Model.Discussion
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


-- TODO: make this look not awful
newWikiTranslationForm :: Maybe WikiEditId -> Maybe Markdown -> Maybe Language -> Maybe Text -> Maybe Bool -> Form (WikiEditId, Markdown, Language, Text, Bool)
newWikiTranslationForm wiki_edit_id content language target complete = renderBootstrap3 BootstrapBasicForm $ (,,,,)
    <$> areq' hiddenField "" wiki_edit_id
    <*> areq' snowdriftMarkdownField "Page Content" content
    <*> areq' (selectField makeLanguageOptions) "Language" language
    <*> areq' textField "Target" target
    <*> areq' boolField "Complete Translation" complete



renderWiki :: Int -> Text -> Language -> Text -> Bool -> WikiEdit -> Widget
renderWiki comment_count project_handle language target can_edit wiki_edit = do
    let wiki_target_id = Key $ PersistInt64 (-1)
        wiki_target = WikiTarget
            (error "attempted to access page field of fake wiki target")
            (error "attempted to access project field of fake wiki target")
            target
            language

        discussion = DiscussionOnWikiPage (Entity wiki_target_id wiki_target)

    $(widgetFile "wiki")
