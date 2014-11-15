module View.Wiki where

import Import

import           Model.Discussion
import           Model.Permission
import           Model.Markdown
import           Widgets.Markdown

import Data.List ((\\))

editWikiForm :: WikiEditId -> Markdown -> Maybe Text -> Form (WikiEditId, Markdown, Text, Language)
editWikiForm last_edit_id content comment html = do
    languages <- lift getLanguages

    let language_options = map (id &&& id) $ languages ++ ([minBound..maxBound] \\ languages)

        form = renderBootstrap3 BootstrapBasicForm $ (,,,)
                <$> areq' hiddenField "" (Just last_edit_id)
                <*> areq' snowdriftMarkdownField "Page Content" (Just content)
                <*> areq' textField "Comment" comment
                <*> areq' (selectFieldList language_options) "Language" (listToMaybe languages)

    form html

editWikiPermissionsForm :: PermissionLevel -> Form PermissionLevel
editWikiPermissionsForm level = renderBootstrap3 BootstrapBasicForm $ areq permissionLevelField "Permission Level" (Just level)

newWikiForm :: Maybe Markdown -> Form Markdown
newWikiForm content = renderBootstrap3 BootstrapBasicForm $ areq' snowdriftMarkdownField "Page Content" content

renderWiki :: Int -> Text -> Language -> Text -> Bool -> WikiEdit -> Widget
renderWiki comment_count project_handle language target can_edit wiki_edit = do
    now <- liftIO getCurrentTime
    let wiki_page_id = Key $ PersistInt64 (-1)
        wiki_page = WikiPage now
            (error "attempted to access project field of fake wiki page")
            (error "attempted to access discussion field of fake wiki page")
            (error "attempted to access permission level field of fake wiki page")

        wiki_target_id = Key $ PersistInt64 (-1)
        wiki_target = WikiTarget
            (error "attempted to access page field of fake wiki target")
            (error "attempted to access project field of fake wiki target")
            target
            language

        discussion = DiscussionOnWikiPage (Entity wiki_page_id wiki_page) (Entity wiki_target_id wiki_target)

    $(widgetFile "wiki")
