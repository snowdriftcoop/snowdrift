module Handler.Tags where

import Import

import Model.User
import Model.AnnotatedTag


import Widgets.Tag

import Model.WikiPage

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

processCommentTags :: ([AnnotatedTag] -> Handler Html) -> Text -> Text -> CommentId -> Handler Html
processCommentTags go project_handle target comment_id = do
    (Entity project_id _, Entity page_id _) <- getPageInfo project_handle target

    comment_page_id <- runDB $ getCommentPageId comment_id

    when (comment_page_id /= page_id) $ error "wrong page for comment"

    tags <- fmap (map $ (commentTagTag &&& (commentTagUser &&& commentTagCount)) . entityVal) $ runDB $ select $ from $ \ comment_tag -> do
        where_ $ comment_tag ^. CommentTagComment ==. val comment_id
        return comment_tag

    tag_map <- fmap (M.fromList . entityPairs) $ runDB $ select $ from $ \ tag -> do
        where_ $ tag ^. TagId `in_` valList (S.toList $ S.fromList $ map fst tags)
        return tag

    go =<< buildAnnotatedTags tag_map (CommentTagR project_handle target comment_id) tags


processCommentTag :: (AnnotatedTag -> Handler Html) -> Text -> Text -> CommentId -> TagId -> Handler Html
processCommentTag go project_handle target comment_id tag_id = do
    (Entity project_id _, Entity page_id _) <- getPageInfo project_handle target

    comment_page <- runDB $ getCommentPageId comment_id

    when (comment_page /= page_id) $ error "wrong page for comment"

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


getOldCommentTagR :: Text -> Text -> CommentId -> TagId -> Handler Html
getOldCommentTagR project_handle target comment_id tag_id = redirect $ OldCommentTagR project_handle target comment_id tag_id

getCommentTagR :: Text -> Text -> CommentId -> TagId -> Handler Html
getCommentTagR = processCommentTag $ \ (AnnotatedTag tag url color user_votes) -> do
    let tag_name = tagName $ entityVal tag

    (incForm, _) <- generateFormPost $ tagBumpForm 1
    (decForm, _) <- generateFormPost $ tagBumpForm (-1)

    defaultLayout $(widgetFile "tag")


postOldCommentTagR :: Text -> Text -> CommentId -> TagId -> Handler Html
postOldCommentTagR = postCommentTagR

postCommentTagR :: Text -> Text -> CommentId -> TagId -> Handler Html
postCommentTagR project_handle target comment_id tag_id = do
    user_id <- requireAuthId

    (Entity project_id _, Entity page_id _) <- getPageInfo project_handle target

    comment_page_id <- runDB $ getCommentPageId comment_id

    when (comment_page_id /= page_id) $ error "wrong page for comment"

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


getOldCommentTagsR :: Text -> Text -> CommentId -> Handler Html
getOldCommentTagsR project_handle target comment_id = redirect $ CommentTagsR project_handle target comment_id

getCommentTagsR :: Text -> Text -> CommentId -> Handler Html
getCommentTagsR = processCommentTags renderTags

createCommentTagForm :: Form (Text, Text)
createCommentTagForm = renderBootstrap3 $ (,)
    <$> areq textField "" Nothing
    <*> areq hiddenField "" (Just "create")

newCommentTagForm :: [Entity Tag] -> Form (TagId, Text)
newCommentTagForm all_tags = renderBootstrap3 $ (,)
    <$> areq (selectFieldList tags) "" Nothing
    <*> areq hiddenField "" (Just "apply")
    where tags = fmap (\(Entity tag_id tag) -> (tagName tag, tag_id)) all_tags

getOldNewCommentTagR :: Text -> Text -> CommentId -> Handler Html
getOldNewCommentTagR project_handle target comment_id = redirect $ NewCommentTagR project_handle target comment_id

getNewCommentTagR :: Text -> Text -> CommentId -> Handler Html
getNewCommentTagR project_handle target comment_id = do
    Entity user_id user <- requireAuth

    when (not $ isJust $ userEstablishedTs user) (permissionDenied "You must be an established user to add tags")

    Entity project_id _ <- runDB $ getBy404 $ UniqueProjectHandle project_handle
    Entity page_id _ <- runDB $ getBy404 $ UniqueWikiTarget project_id target

    comment <- runDB $ get404 comment_id
    comment_page_id <- runDB $ getCommentPageId comment_id

    when (comment_page_id /= page_id) $ error "wrong page for comment"

    comment_tags <- fmap (map entityVal) $ runDB $ select $ from $ \ comment_tag -> do
        where_ $ comment_tag ^. CommentTagComment ==. val comment_id
        return comment_tag

    tag_map <- fmap (M.fromList . entityPairs) $ runDB $ select $ from $ \ tag -> do
        where_ $ tag ^. TagId `in_` valList (S.toList $ S.fromList $ map commentTagTag comment_tags)
        return tag

    tags <- annotateCommentTags tag_map project_handle target comment_id comment_tags

    all_tags :: [Entity Tag] <- runDB $ select $ from $ \ tag -> do
        orderBy [ desc (tag ^. TagName) ]
        return tag

    (apply_form, _) <- generateFormPost $ newCommentTagForm all_tags
    (create_form, _) <- generateFormPost $ createCommentTagForm

    defaultLayout $(widgetFile "new_comment_tag")


postOldNewCommentTagR :: Text -> Text -> CommentId -> Handler Html
postOldNewCommentTagR = postNewCommentTagR False

postCreateNewCommentTagR = postNewCommentTagR True
postApplyNewCommentTagR = postNewCommentTagR False

postNewCommentTagR :: Bool -> Text -> Text -> CommentId -> Handler Html
postNewCommentTagR create_tag project_handle target comment_id = do
    user_id <- requireAuthId

    when (not $ isJust $ userEstablishedTs user) (permissionDenied "You must be an established user to add tags")

    (Entity project_id _, Entity page_id _) <- getPageInfo project_handle target


    comment_page_id <- runDB $ getCommentPageId comment_id

    when (comment_page_id /= page_id) $ error "wrong page for comment"

    all_tags :: [Entity Tag] <- runDB $ select $ from $ \ tag -> do
        orderBy [ desc (tag ^. TagName) ]
        return tag

    let formFailure es = error $ T.unpack $ "form submission failed: " <> T.intercalate "; " es

    if create_tag
        then do
            ((result_create, _), _) <- runFormPost $ createCommentTagForm
            case result_create of 
                FormSuccess (tag_name, _) -> do
                    runDB $ do
                        maybe_tag <- getBy $ UniqueTag tag_name
                        case maybe_tag of
                            Nothing -> do 
                                tag_id <- insert $ Tag tag_name
                                insert $ CommentTag comment_id tag_id user_id 1
                            Just _ -> permissionDenied "tag already exists"
                    redirectUltDest $ DiscussCommentR project_handle target comment_id
                FormMissing -> error "form missing"
                FormFailure es -> formFailure es
        else do
            ((result_apply, _), _) <- runFormPost $ newCommentTagForm all_tags
            case result_apply of 
                FormSuccess (tag_id, _) -> do
                    runDB $ do
                        maybe_tag <- get tag_id
                        case maybe_tag of
                            Nothing -> permissionDenied "tag does not exist"
                            Just _ -> void $ insert $ CommentTag comment_id tag_id user_id 1
                    redirectUltDest $ DiscussCommentR project_handle target comment_id
                FormMissing -> error "form missing"
                FormFailure es -> formFailure (es <> [T.pack " apply"])

{-
    case result of
        FormMissing -> error "form missing"
        FormFailure es -> error $ T.unpack $ "form submission failed: " <> T.intercalate "; " es
        FormSuccess tag_id -> do
            runDB $ do
                maybe_tag <- get tag_id
                case maybe_tag of
                    {-
                    Nothing -> insert $ Tag tag_name
                    -}
                    Nothing -> permissionDenied "tag does not exist"
                    Just _ ->
                        void $ insert $ CommentTag comment_id tag_id user_id 1

            redirectUltDest $ DiscussCommentR project_handle target comment_id
-}
