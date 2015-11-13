module View.Wiki where

import Import

import qualified Data.Text as T

import DeprecatedBootstrap
import Handler.Utils
import Model.Markdown
import Model.Permission.Internal
import Widgets.Markdown

editWikiForm :: WikiEditId -> Markdown -> Maybe Text -> Form (WikiEditId, Markdown, Text)
editWikiForm last_edit_id content comment = renderBootstrap3 BootstrapBasicForm $ (,,)
    <$> areq' hiddenField "" (Just last_edit_id)
    <*> areq' snowdriftMarkdownField "Page Content" (Just content)
    <*> areq' textField "Comment" comment

permissionLevelField
    :: (RenderMessage (HandlerSite m) FormMessage, m ~ HandlerT site IO)
    => Field m PermissionLevel
permissionLevelField =
    (radioField' . optionsPairs) $
        map (permissionLevelLabel &&& id) [minBound ..]

permissionLevelLabel :: PermissionLevel -> Text
permissionLevelLabel = T.pack . show


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
    -- This should be a 'boolField', but it's not rendered properly.
    <*> areq' (selectFieldList yesno) "Complete translation" complete
  where
    yesno :: [(Text, Bool)]
    yesno = [("Yes", True), ("No", False)]



data RenderWikiArgs = RenderWikiArgs
    { comment_count :: Int
    , project_handle :: Text
    , language :: Language
    , target :: Text
    , can_edit :: Bool
    , lang_param :: Maybe Text
    , translations :: [Language]
    , wiki_edit :: WikiEdit
    }

renderWiki :: RenderWikiArgs -> Widget
renderWiki RenderWikiArgs{..} = do
    let wiki_target_id = key $ PersistInt64 (-1)
        wiki_target = WikiTarget
            (error "attempted to access page field of fake wiki target")
            (error "attempted to access project field of fake wiki target")
            target
            language

        discussion = DiscussionOnWikiPage (Entity wiki_target_id wiki_target)

    $(widgetFile "wiki")
