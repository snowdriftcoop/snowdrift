module Handler.Tags where

import Import

import qualified Data.Map           as M
import qualified Data.Set           as S
import qualified Data.Text          as T

import           Model.AnnotatedTag
import           Model.Comment      (getCommentPageId)
import           Model.User
import           Model.WikiPage
import           Widgets.Tag

processCommentTags :: ([AnnotatedTag] -> Handler Html) -> Text -> Text -> CommentId -> Handler Html
processCommentTags go project_handle target comment_id = do
    (_, Entity page_id _) <- getPageInfo project_handle target

    comment_page_id <- runDB $ getCommentPageId comment_id

    when (comment_page_id /= page_id) $ error "wrong page for comment"

    tags <- fmap (map $ (commentTagTag &&& (commentTagUser &&& commentTagCount)) . entityVal) $
        runDB $ select $ from $ \ comment_tag -> do
            where_ $ comment_tag ^. CommentTagComment ==. val comment_id
            return comment_tag

    tag_map <- fmap (M.fromList . entityPairs) $ runDB $ select $ from $ \ tag -> do
        where_ $ tag ^. TagId `in_` valList (S.toList $ S.fromList $ map fst tags)
        return tag

    go =<< buildAnnotatedTags tag_map (CommentTagR project_handle target comment_id) tags


processCommentTag :: (AnnotatedTag -> Handler Html) -> Text -> Text -> CommentId -> TagId -> Handler Html
processCommentTag go project_handle target comment_id tag_id = do
    (_, Entity page_id _) <- getPageInfo project_handle target

    comment_page <- runDB $ getCommentPageId comment_id

    when (comment_page /= page_id) $ error "wrong page for comment"

    tags <- fmap (map $ (commentTagTag &&& (commentTagUser &&& commentTagCount)) . entityVal) $
        runDB $ select $ from $ \ comment_tag -> do
            where_ $ comment_tag ^. CommentTagComment ==. val comment_id
                &&. comment_tag ^. CommentTagTag ==. val tag_id
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

    (_, Entity page_id _) <- getPageInfo project_handle target

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


getCommentTagsR :: Text -> Text -> CommentId -> Handler Html
getCommentTagsR = processCommentTags renderTags

createCommentTagForm :: Form Text
createCommentTagForm = renderBootstrap3 $ areq textField "" Nothing
--    <*> areq hiddenField "" (Just "create")

newCommentTagForm :: [Entity Tag] -> [Entity Tag] -> Form (Maybe [TagId], Maybe [TagId])
newCommentTagForm project_tags other_tags = renderBootstrap3 $ (,)
    -- <$> fmap (\(Entity tag_id tag) -> aopt checkBoxField (tag_id) (tagName tag)) (project_tags <> other_tags)
    <$> aopt (tagCloudField $ tags project_tags) "Tags used elsewhere in this project:" Nothing
    <*> aopt (tagCloudField $ tags other_tags) "Tags used in other projects:" Nothing
--    <*> areq hiddenField "" (Just "apply")
    where tags = fmap (\(Entity tag_id tag) -> (tagName tag, tag_id))
          tagCloudField = checkboxesFieldList' $ (\(PersistInt64 a) -> show a) . unKey


tagList :: ProjectId -> Handler ([Entity Tag], [Entity Tag])
tagList project_id = do
    project_tags :: [Entity Tag] <- runDB $
        selectDistinct $ from $ \(tag `InnerJoin` rel `InnerJoin` comment `InnerJoin` page) -> do
            on_ ( page ^. WikiPageDiscussion ==. comment ^. CommentDiscussion )
            on_ ( comment ^. CommentId ==. rel ^. CommentTagComment )
            on_ ( rel ^. CommentTagTag ==. tag ^. TagId )
            where_ ( page ^. WikiPageProject ==. val project_id )
            orderBy [ desc (tag ^. TagName) ]
            return tag

    other_tags :: [Entity Tag] <- runDB $
        selectDistinct $ from $ \(tag `InnerJoin` rel `InnerJoin` comment `InnerJoin` page) -> do
            on_ ( page ^. WikiPageDiscussion ==. comment ^. CommentDiscussion )
            on_ ( comment ^. CommentId ==. rel ^. CommentTagComment )
            on_ ( rel ^. CommentTagTag ==. tag ^. TagId )
            where_ ( page ^. WikiPageProject !=. val project_id )
            orderBy [ desc (tag ^. TagName) ]
            return tag

    return (project_tags, other_tags)

getNewCommentTagR :: Text -> Text -> CommentId -> Handler Html
getNewCommentTagR project_handle target comment_id = do
    Entity user_id user <- requireAuth

    unless (isJust $ userEstablishedTs user)
        (permissionDenied "You must be an established user to add tags")

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

    (project_tags, other_tags) <- tagList project_id
{-
    project_tags :: [Entity Tag] <- runDB $ select $ from $ \(tag `InnerJoin` rel `InnerJoin` comment `InnerJoin` page) -> do
        on_ ( page ^. WikiPageDiscussion ==. comment ^. CommentDiscussion )
        on_ ( comment ^. CommentId ==. rel ^. CommentTagComment )
        on_ ( rel ^. CommentTagTag ==. tag ^. TagId )
        where_ ( page ^. WikiPageProject ==. val project_id )
        orderBy [ desc (tag ^. TagName) ]
        return tag

    other_tags :: [Entity Tag] <- runDB $ select $ from $ \(tag `InnerJoin` rel `InnerJoin` comment `InnerJoin` page) -> do
        on_ ( page ^. WikiPageDiscussion ==. comment ^. CommentDiscussion )
        on_ ( comment ^. CommentId ==. rel ^. CommentTagComment )
        on_ ( rel ^. CommentTagTag ==. tag ^. TagId )
        where_ ( page ^. WikiPageProject !=. val project_id )
        orderBy [ desc (tag ^. TagName) ]
        return tag

    all_tags :: [Entity Tag] <- runDB $ select $ from $ \ tag -> do
        orderBy [ desc (tag ^. TagName) ]
        return tag
-}

    let filter_tags = filter (\(Entity t _) -> not $ M.member t tag_map)
    (apply_form, _) <- generateFormPost $ newCommentTagForm (filter_tags project_tags) (filter_tags other_tags)
    (create_form, _) <- generateFormPost $ createCommentTagForm

    defaultLayout $(widgetFile "new_comment_tag")


postCreateNewCommentTagR = postNewCommentTagR True
postApplyNewCommentTagR = postNewCommentTagR False

postNewCommentTagR :: Bool -> Text -> Text -> CommentId -> Handler Html
postNewCommentTagR create_tag project_handle target comment_id = do
    Entity user_id user <- requireAuth

    unless (isJust $ userEstablishedTs user)
        (permissionDenied "You must be an established user to add tags")

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
                FormSuccess (tag_name) -> do
                    msuccess <- runDB $ do
                        maybe_tag <- getBy $ UniqueTag tag_name
                        case maybe_tag of
                            Nothing -> do
                                tag_id <- insert $ Tag tag_name
                                void $ insert $ CommentTag comment_id tag_id user_id 1
                            Just _ -> do
                                return ()
                        return maybe_tag
                    if (isJust $ msuccess) then do
                        addAlert "danger" "that tag already exists"
                        redirectUltDest $ NewCommentTagR project_handle target comment_id
                        else do
                            redirectUltDest $ DiscussCommentR project_handle target comment_id
                FormMissing -> error "form missing"
                FormFailure es -> formFailure es
        else do
            comment_tags <- fmap (map entityVal) $ runDB $ select $ from $ \ comment_tag -> do
                where_ $ comment_tag ^. CommentTagComment ==. val comment_id
                return comment_tag

            tag_map <- fmap (M.fromList . entityPairs) $ runDB $ select $ from $ \ tag -> do
                where_ $ tag ^. TagId `in_` valList (S.toList $ S.fromList $ map commentTagTag comment_tags)
                return tag
            let filter_tags = filter (\(Entity t _) -> not $ M.member t tag_map)
            (project_tags, other_tags) <- tagList project_id
            ((result_apply, _), _) <- runFormPost $ newCommentTagForm (filter_tags project_tags) (filter_tags other_tags)
            case result_apply of
                FormSuccess (mproject_tag_ids, mother_tag_ids) -> do
                    let project_tag_ids = fromMaybe [] mproject_tag_ids
                    let other_tag_ids = fromMaybe [] mother_tag_ids
                    runDB $ do
                        let tag_ids = project_tag_ids <> other_tag_ids
                        valid_tags <- select $ from $ \tag -> do
                            where_ ( tag ^. TagId `in_` valList tag_ids )
                            return tag
                        if (null valid_tags)
                            then
                                permissionDenied "error: invalid tag id"
                            else
                                void $ insertMany $ fmap (\(Entity tag_id _) -> CommentTag comment_id tag_id user_id 1) valid_tags
                        -- case maybe_tag of
                        --    Nothing -> permissionDenied "tag does not exist"
                        --    Just _ -> void $ insert $ CommentTag comment_id tag_id user_id 1
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

