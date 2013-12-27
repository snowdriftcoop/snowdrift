module Handler.Tags where

import Import

import Model.User
import Model.AnnotatedTag

import Widgets.Sidebar
import Widgets.Tag

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

processCommentTags :: ([AnnotatedTag] -> Handler Html) -> Text -> Text -> CommentId -> Handler Html
processCommentTags go project_handle target comment_id = do
    Entity project_id _ <- runDB $ getBy404 $ UniqueProjectHandle project_handle
    Entity page_id _ <- runDB $ getBy404 $ UniqueWikiTarget project_id target

    comment <- runDB $ get404 comment_id

    when (commentPage comment /= page_id) $ error "wrong page for comment"

    tags <- fmap (map $ (commentTagTag &&& (commentTagUser &&& commentTagCount)) . entityVal) $ runDB $ select $ from $ \ comment_tag -> do
        where_ $ comment_tag ^. CommentTagComment ==. val comment_id
        return comment_tag

    tag_map <- fmap (M.fromList . entityPairs) $ runDB $ select $ from $ \ tag -> do
        where_ $ tag ^. TagId `in_` valList (S.toList $ S.fromList $ map fst tags)
        return tag

    go =<< buildAnnotatedTags tag_map (CommentTagR project_handle target comment_id) tags


processCommentTag :: (AnnotatedTag -> Handler Html) -> Text -> Text -> CommentId -> TagId -> Handler Html
processCommentTag go project_handle target comment_id tag_id = do
    Entity project_id _ <- runDB $ getBy404 $ UniqueProjectHandle project_handle
    Entity page_id _ <- runDB $ getBy404 $ UniqueWikiTarget project_id target

    comment <- runDB $ get404 comment_id

    when (commentPage comment /= page_id) $ error "wrong page for comment"

    tags <- fmap (map $ (commentTagTag &&& (commentTagUser &&& commentTagCount)) . entityVal) $ runDB $ select $ from $ \ comment_tag -> do
        where_ $ comment_tag ^. CommentTagComment ==. val comment_id &&. comment_tag ^. CommentTagTag ==. val tag_id
        return comment_tag

    tag_map <- fmap (M.fromList . entityPairs) $ runDB $ select $ from $ \ tag -> do
        where_ $ tag ^. TagId `in_` valList (S.toList $ S.fromList $ map fst tags)
        return tag

    annotated_tags <- buildAnnotatedTags tag_map (CommentTagR project_handle target comment_id) tags
    case annotated_tags of
        [] -> error "That tag has not been applied to this comment."
        [tag] -> go tag
        _ -> error "This should never happen."

renderTags :: [AnnotatedTag] -> Handler Html
renderTags tags = defaultLayout $(widgetFile "tags")


tagBumpForm :: Int -> Form Int
tagBumpForm = renderDivs . areq hiddenField "" . Just

getCommentTagR :: Text -> Text -> CommentId -> TagId -> Handler Html
getCommentTagR = processCommentTag $ \ (AnnotatedTag tag url color user_votes) -> do
    let tag_name = tagName $ entityVal tag

    (incForm, _) <- generateFormPost $ tagBumpForm 1
    (decForm, _) <- generateFormPost $ tagBumpForm (-1)

    defaultLayout $(widgetFile "tag")

postCommentTagR :: Text -> Text -> CommentId -> TagId -> Handler Html
postCommentTagR project_handle target comment_id tag_id = do
    user_id <- requireAuthId
    Entity project_id _ <- runDB $ getBy404 $ UniqueProjectHandle project_handle
    Entity page_id _ <- runDB $ getBy404 $ UniqueWikiTarget project_id target

    comment <- runDB $ get404 comment_id

    when (commentPage comment /= page_id) $ error "wrong page for comment"

    direction <- lookupPostParam "direction"

    let delta = case T.unpack <$> direction of
            Just "+" -> 1
            Just "-" -> -1
            Just "\215" -> -1
            Nothing -> error "direction unset"
            Just str -> error $ "unrecognized direction: " ++ str

    runDB $ do
        maybe_comment_tag_entity <- getBy $ UniqueCommentTag comment_id tag_id user_id
        case maybe_comment_tag_entity of
            Nothing -> void $ insert $ CommentTag comment_id tag_id user_id delta
            Just (Entity comment_tag_id comment_tag) -> case commentTagCount comment_tag + delta of
                0 -> delete $ from $ \ ct -> where_ $ ct ^. CommentTagId ==. val comment_tag_id
                x -> void $ update $ \ ct -> do
                    set ct [ CommentTagCount =. val x ]
                    where_ $ ct ^. CommentTagId ==. val comment_tag_id

    setUltDestReferer
    redirectUltDest $ CommentTagR project_handle target comment_id tag_id


getCommentTagsR :: Text -> Text -> CommentId -> Handler Html
getCommentTagsR = processCommentTags renderTags


newCommentTagForm :: Form Text
newCommentTagForm = renderBootstrap3 $ areq textField "" Nothing

getNewCommentTagR :: Text -> Text -> CommentId -> Handler Html
getNewCommentTagR project_handle target comment_id = do
    Entity project_id _ <- runDB $ getBy404 $ UniqueProjectHandle project_handle
    Entity page_id _ <- runDB $ getBy404 $ UniqueWikiTarget project_id target

    comment <- runDB $ get404 comment_id

    when (commentPage comment /= page_id) $ error "wrong page for comment"

    comment_tags <- fmap (map entityVal) $ runDB $ select $ from $ \ comment_tag -> do
        where_ $ comment_tag ^. CommentTagComment ==. val comment_id
        return comment_tag

    tag_map <- fmap (M.fromList . entityPairs) $ runDB $ select $ from $ \ tag -> do
        where_ $ tag ^. TagId `in_` valList (S.toList $ S.fromList $ map commentTagTag comment_tags)
        return tag

    tags <- annotateCommentTags tag_map project_handle target comment_id comment_tags

    (form, _) <- generateFormPost newCommentTagForm

    defaultLayout $(widgetFile "new_comment_tag")

postNewCommentTagR :: Text -> Text -> CommentId -> Handler Html
postNewCommentTagR project_handle target comment_id = do
    user_id <- requireAuthId

    Entity project_id _ <- runDB $ getBy404 $ UniqueProjectHandle project_handle
    Entity page_id _ <- runDB $ getBy404 $ UniqueWikiTarget project_id target

    comment <- runDB $ get404 comment_id

    when (commentPage comment /= page_id) $ error "wrong page for comment"

    ((result, _), _) <- runFormPost newCommentTagForm

    case result of
        FormMissing -> error "form missing"
        FormFailure es -> error $ T.unpack $ "form submission failed: " <> T.intercalate "; " es
        FormSuccess tag_name -> do
            runDB $ do
                maybe_tag <- getBy $ UniqueTag tag_name
                tag_id <- case maybe_tag of
                    Nothing -> insert $ Tag tag_name
                    Just (Entity tag_id _) -> return tag_id

                void $ insert $ CommentTag comment_id tag_id user_id 1

            redirectUltDest $ DiscussCommentR project_handle target comment_id
