{-# LANGUAGE TupleSections #-}

module Handler.Wiki where

import Import


import Widgets.Markdown
import Widgets.Time
import Widgets.Tag
import Widgets.Preview

import Model.Permission
import Model.User
import Model.AnnotatedTag

import Yesod.Markdown
import Model.Markdown

import qualified Data.Text as T

import qualified Data.Foldable as F

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.List as L

import Data.Tree

import Data.Time

import Data.Algorithm.Diff (getDiff, Diff (..))

import Text.Blaze.Html5 (ins, del, br)

getWikiR :: Text -> Text -> Handler Html
getWikiR project_handle target = do
    maybe_user_id <- maybeAuthId

    let can_view_meta = isJust maybe_user_id

    (page, can_edit) <- runDB $ do
        Entity project_id _ <- getBy404 $ UniqueProjectHandle project_handle
        Entity _ page <- getBy404 $ UniqueWikiTarget project_id target

        -- TODO this should be changed when we add page moderation
        can_edit <- case maybe_user_id of
            Nothing -> return False
            Just user_id -> (||)
                <$> isProjectAdmin project_handle user_id
                <*> isProjectAdmin "snowdrift" user_id

        return (page, can_edit)

    defaultLayout $ renderWiki project_handle target can_edit can_view_meta page


getOldWikiPagesR :: Text -> Handler Html
getOldWikiPagesR = redirect . WikiPagesR

getWikiPagesR :: Text -> Handler Html
getWikiPagesR project_handle = do
    pages <- runDB $ select $ from $ \ (project `InnerJoin` wiki_page) -> do
        on_ $ project ^. ProjectId ==. wiki_page ^. WikiPageProject
        where_ $ project ^. ProjectHandle ==. val project_handle
        orderBy [asc $ wiki_page ^. WikiPageTarget]
        return wiki_page

    defaultLayout $(widgetFile "wiki_pages")


renderWiki :: Text -> Text -> Bool -> Bool -> WikiPage -> Widget
renderWiki project_handle target can_edit can_view_meta page = $(widgetFile "wiki")


postOldWikiR :: Text -> Text -> Handler Html
postOldWikiR = postWikiR

postWikiR :: Text -> Text -> Handler Html
postWikiR project_handle target = do
    Entity user_id _ <- requireAuth
    now <- liftIO getCurrentTime

    affiliated <- runDB $ (||)
            <$> isProjectAffiliated project_handle user_id
            <*> isProjectAdmin "snowdrift" user_id

    when (not affiliated) $ permissionDenied "you do not have permission to edit this page"

    (project_id, Entity page_id page, Entity _ last_edit) <- runDB $ do
        Entity project_id _ <- getBy404 $ UniqueProjectHandle project_handle
        page_entity <- getBy404 $ UniqueWikiTarget project_id target
        last_edit_entity <- getBy404 $ UniqueWikiLastEdit $ entityKey page_entity
        return (project_id, page_entity, last_edit_entity)

    ((result, _), _) <- runFormPost $ editWikiForm undefined (wikiPageContent page) Nothing


    case result of
        FormSuccess (last_edit_id, content, comment) -> do
            mode <- lookupPostParam "mode"

            let action :: Text = "update"

            case mode of
                Just "preview" -> do
                    (form, _) <- generateFormPost $ editWikiForm last_edit_id content comment
                    defaultLayout $ renderPreview form action $ renderWiki project_handle target False False $ WikiPage target project_id content Normal

                Just x | x == action -> do
                    runDB $ do
                        update $ \ p -> do
                            set p [WikiPageContent =. val content]
                            where_ $ p ^. WikiPageId ==. val page_id

                        edit_id <- insert $ WikiEdit now user_id page_id content comment
                        -- TODO - I think there might be a race condition here...
                        either_last_edit <- insertBy $ WikiLastEdit page_id edit_id

                        if last_edit_id == wikiLastEditEdit last_edit
                         then lift $ addAlert "success" "Updated."
                         else do
                            [ Value last_editor ] <- select $ from $ \ edit -> do
                                where_ $ edit ^. WikiEditId ==. val (wikiLastEditEdit last_edit)
                                return $ edit ^. WikiEditUser

                            let comment_body = Markdown $ T.unlines
                                    [ "ticket: edit conflict"
                                    , ""
                                    , "[original version](/w/" <> target <> "/history/" <> toPathPiece last_edit_id <> ")"
                                    , ""
                                    , "[my version](/w/" <> target <> "/history/" <> toPathPiece edit_id <> ")"
                                    , ""
                                    , "[their version](/w/" <> target <> "/history/" <> (toPathPiece $ wikiLastEditEdit last_edit) <> ")"
                                    , ""
                                    , "(this ticket was automatically generated)"
                                    ]

                            comment_id <- insert $ Comment now (Just now) (Just user_id) page_id Nothing user_id comment_body 0

                            void $ insert $ Ticket now "edit conflict" comment_id

                            render <- lift getUrlRenderParams
                            let message_text = Markdown $ T.unlines
                                    [ "Edit conflict for wiki page \"" <> target <> "\"."
                                    , "Ticket created: " <> render (DiscussCommentR project_handle target comment_id) []
                                    , "(this message was automatically generated)"
                                    ]

                            void $ insert $ Message (Just project_id) now (Just last_editor) (Just user_id) message_text
                            void $ insert $ Message (Just project_id) now (Just user_id) (Just last_editor) message_text

                            lift $ addAlert "danger" "conflicting edits (ticket created, messages sent)"

                        case either_last_edit of
                            Left (Entity to_update _) -> update $ \ l -> do
                                set l [WikiLastEditEdit =. val edit_id]
                                where_ $ l ^. WikiLastEditId ==. val to_update

                            Right _ -> return ()

                    redirect $ WikiR project_handle target

                _ -> error "Error: unrecognized mode"


        FormMissing -> error "Form missing."
        FormFailure msgs -> error $ "Error submitting form: " ++ T.unpack (T.concat msgs)


editWikiPermissionsForm :: PermissionLevel -> Form PermissionLevel
editWikiPermissionsForm level = renderBootstrap3 $ areq' permissionLevelField "Permission Level" (Just level)

getOldEditWikiPermissionsR :: Text -> Text -> Handler Html
getOldEditWikiPermissionsR project_handle target = redirect $ EditWikiPermissionsR project_handle target

getEditWikiPermissionsR :: Text -> Text -> Handler Html
getEditWikiPermissionsR project_handle target = do
    Entity user_id user <- requireAuth
    (Entity page_id page) <- runDB $ do
        Entity project_id _ <- getBy404 $ UniqueProjectHandle project_handle
        getBy404 $ UniqueWikiTarget project_id target

    affiliated <- runDB $ (||)
            <$> isProjectAdmin project_handle user_id
            <*> isProjectAdmin "snowdrift" user_id

    when (not affiliated) $ permissionDenied "you do not have permission to edit page permissions"

    (wiki_form, _) <- generateFormPost $ editWikiPermissionsForm (wikiPagePermissionLevel page)

    defaultLayout $(widgetFile "edit_wiki_perm")


postOldEditWikiPermissionsR :: Text -> Text -> Handler Html
postOldEditWikiPermissionsR = postEditWikiPermissionsR

postEditWikiPermissionsR :: Text -> Text -> Handler Html
postEditWikiPermissionsR project_handle target = do
    Entity user_id _ <- requireAuth
    (Entity page_id page) <- runDB $ do
        Entity project_id _ <- getBy404 $ UniqueProjectHandle project_handle
        getBy404 $ UniqueWikiTarget project_id target

    affiliated <- runDB $ (||)
            <$> isProjectAdmin project_handle user_id
            <*> isProjectAdmin "snowdrift" user_id

    when (not affiliated) $ permissionDenied "you do not have permission to edit page permissions"

    ((result, _), _) <- runFormPost $ editWikiPermissionsForm (wikiPagePermissionLevel page)

    case result of
        FormSuccess level -> do
            runDB $ update $ \ p -> do
                where_ $ p ^. WikiPageId ==. val page_id
                set p [ WikiPagePermissionLevel =. val level ]

            addAlert "success" "permissions updated"

            redirect $ WikiR project_handle target

        FormMissing -> error "Form missing."
        FormFailure msgs -> error $ "Error submitting form: " ++ T.unpack (T.concat msgs)


getOldEditWikiR :: Text -> Text -> Handler Html
getOldEditWikiR project_handle target = redirect $ EditWikiR project_handle target

getEditWikiR :: Text -> Text -> Handler Html
getEditWikiR project_handle target = do
    Entity user_id user <- requireAuth
    (Entity page_id page, Entity _ last_edit) <- runDB $ do
        Entity project_id _ <- getBy404 $ UniqueProjectHandle project_handle
        page_entity <- getBy404 $ UniqueWikiTarget project_id target
        last_edit_entity <- getBy404 $ UniqueWikiLastEdit $ entityKey page_entity
        return (page_entity, last_edit_entity)

    affiliated <- runDB $ (||)
            <$> isProjectAffiliated project_handle user_id
            <*> isProjectAdmin "snowdrift" user_id

    when (not affiliated) $ permissionDenied "you do not have permission to edit this page"

    (wiki_form, _) <- generateFormPost $ editWikiForm (wikiLastEditEdit last_edit) (wikiPageContent page) Nothing

    defaultLayout $(widgetFile "edit_wiki")


getOldNewWikiR :: Text -> Text -> Handler Html
getOldNewWikiR project_handle target = redirect $ NewWikiR project_handle target

getNewWikiR :: Text -> Text -> Handler Html
getNewWikiR project_handle target = do
    Entity user_id user <- requireAuth

    affiliated <- runDB $ (||)
            <$> isProjectAffiliated project_handle user_id
            <*> isProjectAdmin "snowdrift" user_id

    when (not affiliated) $ permissionDenied "you do not have permission to edit this page"

    (wiki_form, _) <- generateFormPost $ newWikiForm Nothing

    defaultLayout $(widgetFile "new_wiki")


postOldNewWikiR :: Text -> Text -> Handler Html
postOldNewWikiR = postNewWikiR

postNewWikiR :: Text -> Text -> Handler Html
postNewWikiR project_handle target = do
    Entity user_id _ <- requireAuth

    affiliated <- runDB $ (||)
            <$> isProjectAffiliated project_handle user_id
            <*> isProjectAdmin "snowdrift" user_id

    when (not affiliated) $ permissionDenied "you do not have permission to edit this page"

    now <- liftIO getCurrentTime

    Entity project_id _ <- runDB $ getBy404 $ UniqueProjectHandle project_handle

    ((result, _), _) <- runFormPost $ newWikiForm Nothing

    case result of
        FormSuccess content -> do
            mode <- lookupPostParam "mode"
            let action :: Text = "create"
            case mode of
                Just "preview" -> do
                        (form, _) <- generateFormPost $ newWikiForm (Just content)
                        defaultLayout $ renderPreview form action $ renderWiki project_handle target False False page
                            where page = WikiPage target project_id content Normal


                Just x | x == action -> do
                    _ <- runDB $ do
                        page_id <- insert $ WikiPage target project_id content Normal
                        edit_id <- insert $ WikiEdit now user_id page_id content $ Just "Page created."
                        insert $ WikiLastEdit page_id edit_id

                    addAlert "success" "Created."
                    redirect $ WikiR project_handle target

                _ -> error "unrecognized mode"

        FormMissing -> error "Form missing."
        FormFailure msgs -> error $ "Error submitting form: " ++ T.unpack (T.concat msgs)


buildCommentTree :: Entity Comment -> [ Entity Comment ] -> Tree (Entity Comment)
buildCommentTree root rest =
    let treeOfList (node, items) =
            let has_parent p = (== Just (entityKey p)) . commentParent . entityVal
                list = dropWhile (not . has_parent node) items
                (children, rest') = span (has_parent node) list
                items' = map (, rest') children
             in (node, items')

     in unfoldTree treeOfList (root, rest)


getOldDiscussWikiR :: Text -> Text -> Handler Html
getOldDiscussWikiR project_handle target = redirect $ DiscussWikiR project_handle target

getDiscussWikiR :: Text -> Text -> Handler Html
getDiscussWikiR project_handle target = do
    Entity user_id user <- requireAuth
    Entity page_id page  <- runDB $ do
        Entity project_id _ <- getBy404 $ UniqueProjectHandle project_handle
        getBy404 $ UniqueWikiTarget project_id target

    affiliated <- runDB $ (||)
            <$> isProjectAffiliated project_handle user_id
            <*> isProjectAdmin "snowdrift" user_id

    moderator <- runDB $ isProjectModerator project_handle user_id

    (roots, rest, users, retraction_map) <- runDB $ do
        roots <- select $ from $ \ comment -> do
            where_ $ foldl1 (&&.) $ catMaybes
                [ Just $ comment ^. CommentPage ==. val page_id
                , Just $ isNothing $ comment ^. CommentParent
                , if moderator then Nothing else Just $ not_ $ isNothing $ comment ^. CommentModeratedTs
                ]

            orderBy [asc (comment ^. CommentCreatedTs)]
            return comment

        rest <- select $ from $ \ comment -> do
            where_ $ foldl1 (&&.) $ catMaybes
                [ Just $ comment ^. CommentPage ==. val page_id
                , Just $ not_ $ isNothing $ comment ^. CommentParent
                , if moderator then Nothing else Just $ not_ $ isNothing $ comment ^. CommentModeratedTs
                ]

            orderBy [asc (comment ^. CommentParent), asc (comment ^. CommentCreatedTs)]
            return comment

        let get_user_ids = S.fromList . map (commentUser . entityVal) . F.toList
            user_id_list = S.toList $ get_user_ids roots `S.union` get_user_ids rest

        user_entities <- selectList [ UserId <-. user_id_list ] []

        let users = M.fromList $ map (entityKey &&& id) user_entities

        retraction_map <- M.fromList . map ((commentRetractionComment &&& id) . entityVal) <$> selectList [ CommentRetractionComment <-. map entityKey (roots ++ rest) ] []
        return (roots, rest, users, retraction_map)

    tags <- runDB $ select $ from $ return

    let tag_map = M.fromList $ entityPairs tags
        comments = forM_ roots $ \ root ->
            renderComment user_id project_handle target users 10 0 [] retraction_map True tag_map (buildCommentTree root rest) Nothing

    (comment_form, _) <- generateFormPost $ commentForm Nothing Nothing

    let has_comments = not $ null roots

    defaultLayout $(widgetFile "wiki_discuss")


getOldDiscussCommentR :: Text -> Text -> CommentId -> Handler Html
getOldDiscussCommentR project_handle target comment_id = redirect $ DiscussCommentR project_handle target comment_id

getDiscussCommentR :: Text -> Text -> CommentId -> Handler Html
getDiscussCommentR =
    getDiscussCommentR' False

getReplyCommentR :: Text -> Text -> CommentId -> Handler Html
getReplyCommentR =
    getDiscussCommentR' True

getDiscussCommentR' :: Bool -> Text -> Text -> CommentId -> Handler Html
getDiscussCommentR' show_reply project_handle target comment_id = do
    Entity viewer_id _ <- requireAuth
    Entity page_id _  <- runDB $ do
        Entity project_id _ <- getBy404 $ UniqueProjectHandle project_handle
        getBy404 $ UniqueWikiTarget project_id target

    (root, rest, users, earlier_retractions, retraction_map) <- runDB $ do
        root <- get404 comment_id

        when (commentPage root /= page_id) $ error "Selected comment does not match selected page"

        subtree <- select $ from $ \ comment -> do
            where_ ( comment ^. CommentAncestorAncestor ==. val comment_id )
            return comment

        rest <- select $ from $ \ comment -> do
            where_ ( comment ^. CommentPage ==. val page_id
                    &&. comment ^. CommentId >. val comment_id
                    &&. comment ^. CommentId `in_` valList (map (commentAncestorComment . entityVal) subtree))
            orderBy [asc (comment ^. CommentParent), asc (comment ^. CommentCreatedTs)]
            return comment

        let get_user_ids = S.fromList . map (commentUser . entityVal) . F.toList
            user_id_list = S.toList $ S.insert (commentUser root) $ get_user_ids rest

        user_entities <- select $ from $ \ user -> do
            where_ ( user ^. UserId `in_` valList user_id_list )
            return user

        let users = M.fromList $ map (entityKey &&& id) user_entities

        earlier_retractions <- fmap (map entityVal) $ select $ from $ \ (comment_ancestor `InnerJoin` retraction) -> do
            on_ (comment_ancestor ^. CommentAncestorAncestor ==. retraction ^. CommentRetractionComment)
            where_ ( comment_ancestor ^. CommentAncestorComment ==. val comment_id )
            return retraction

        retraction_map <- fmap (M.fromList . map ((commentRetractionComment &&& id) . entityVal)) $ select $ from $ \ retraction -> do
            where_ ( retraction ^. CommentRetractionComment `in_` valList (comment_id : map entityKey rest) )
            return retraction

        return (root, rest, users, earlier_retractions, retraction_map)

    (comment_form, _) <- generateFormPost $ commentForm (Just comment_id) Nothing

    tags <- runDB $ select $ from $ return

    let tag_map = M.fromList $ entityPairs tags

    defaultLayout $ renderDiscussComment viewer_id project_handle target show_reply comment_form (Entity comment_id root) rest users earlier_retractions retraction_map True tag_map


renderDiscussComment :: UserId -> Text -> Text -> Bool -> Widget
    -> Entity Comment -> [Entity Comment]
    -> M.Map UserId (Entity User)
    -> [CommentRetraction]
    -> M.Map CommentId CommentRetraction
    -> Bool -> M.Map TagId Tag -> Widget

renderDiscussComment viewer_id project_handle target show_reply comment_form root rest users earlier_retractions retraction_map show_actions tag_map = do
    let tree = buildCommentTree root rest
        comment = renderComment viewer_id project_handle target users 1 0 earlier_retractions retraction_map show_actions tag_map tree mcomment_form
        mcomment_form =
            if show_reply
                then Just comment_form
                else Nothing

    $(widgetFile "comment")


postOldDiscussWikiR :: Text -> Text -> Handler Html
postOldDiscussWikiR = postDiscussWikiR

postDiscussWikiR :: Text -> Text -> Handler Html
postDiscussWikiR project_handle target = do
    Entity user_id user <- requireAuth
    Entity page_id _ <- runDB $ do
        Entity project_id _ <- getBy404 $ UniqueProjectHandle project_handle
        getBy404 $ UniqueWikiTarget project_id target

    let established = isJust $ userEstablishedTs user

    now <- liftIO getCurrentTime

    ((result, _), _) <- runFormPost $ commentForm Nothing Nothing

    case result of
        FormSuccess (maybe_parent_id, text) -> do
            depth <- case maybe_parent_id of
                Just parent_id -> do
                    Just parent <- runDB $ get parent_id
                    return $ (+1) $ commentDepth parent
                _ -> return 0

            mode <- lookupPostParam "mode"

            let action :: Text = "post"

            case mode of
                Just "preview" -> do
                    earlier_retractions <- runDB $
                        case maybe_parent_id of
                            Just parent_id -> do
                                ancestors <- fmap ((parent_id :) . map (commentAncestorAncestor . entityVal)) $ select $ from $ \ ancestor -> do
                                    where_ ( ancestor ^. CommentAncestorComment ==. val parent_id )
                                    return ancestor

                                fmap (map entityVal) $ select $ from $ \ retraction -> do
                                    where_ ( retraction ^. CommentRetractionComment `in_` valList ancestors )
                                    return retraction

                            Nothing -> return []

                    tags <- runDB $ select $ from $ return

                    let tag_map = M.fromList $ entityPairs tags

                    (form, _) <- generateFormPost $ commentForm maybe_parent_id (Just text)

                    let comment = Entity (Key $ PersistInt64 0) $ Comment now Nothing Nothing page_id maybe_parent_id user_id text depth
                        user_map = M.singleton user_id $ Entity user_id user
                        rendered_comment = renderDiscussComment user_id project_handle target False (return ()) comment [] user_map earlier_retractions M.empty False tag_map

                    defaultLayout $ renderPreview form action rendered_comment


                Just x | x == action -> do
                    runDB $ do
                        comment_id <- insert $ Comment now
                            (if established then Just now else Nothing)
                            (if established then Just user_id else Nothing)
                            page_id maybe_parent_id user_id text depth

                        let content = T.lines $ (\ (Markdown str) -> str) text
                            tickets = map T.strip $ mapMaybe (T.stripPrefix "ticket:") content
                            tags = map T.strip $ mconcat $ map (T.splitOn ",") $ mapMaybe (T.stripPrefix "tags:") content

                        forM_ tickets $ \ ticket -> insert $ Ticket now ticket comment_id
                        forM_ tags $ \ tag -> do
                            tag_id <- fmap (either entityKey id) $ insertBy $ Tag tag
                            insert $ CommentTag comment_id tag_id user_id 1

                        let getParentAncestors parent_id = do
                                comment_ancestor_entities <- select $ from $ \ comment_ancestor -> do
                                    where_ ( comment_ancestor ^. CommentAncestorComment ==. val parent_id )
                                    return comment_ancestor

                                let ancestors = map (commentAncestorAncestor . entityVal) comment_ancestor_entities
                                return $ parent_id : ancestors

                        ancestors <- maybe (return []) getParentAncestors maybe_parent_id

                        forM_ ancestors $ \ ancestor_id -> insert $ CommentAncestor comment_id ancestor_id

                    addAlert "success" $ if established then "comment posted" else "comment submitted for moderation"
                    redirect $ maybe (DiscussWikiR project_handle target) (DiscussCommentR project_handle target) maybe_parent_id

                _ -> error "unrecognized mode"

        FormMissing -> error "Form missing."
        FormFailure msgs -> error $ "Error submitting form: " ++ T.unpack (T.intercalate "\n" msgs)


getOldNewDiscussWikiR :: Text -> Text -> Handler Html
getOldNewDiscussWikiR project_handle target = redirect $ NewDiscussWikiR project_handle target

getNewDiscussWikiR :: Text -> Text -> Handler Html
getNewDiscussWikiR project_handle target = do
    Entity user_id user <- requireAuth
    Entity page_id page  <- runDB $ do
        Entity project_id _ <- getBy404 $ UniqueProjectHandle project_handle
        getBy404 $ UniqueWikiTarget project_id target

    affiliated <- runDB $ (||)
            <$> isProjectAffiliated project_handle user_id
            <*> isProjectAdmin "snowdrift" user_id

    (comment_form, _) <- generateFormPost $ commentForm Nothing Nothing
    
    defaultLayout $(widgetFile "wiki_discuss_new")

postOldNewDiscussWikiR :: Text -> Text -> Handler Html
postOldNewDiscussWikiR = postDiscussWikiR

postNewDiscussWikiR :: Text -> Text -> Handler Html
postNewDiscussWikiR = postDiscussWikiR


getOldWikiNewCommentsR :: Text -> Handler Html
getOldWikiNewCommentsR project_handle = redirect $ WikiNewCommentsR project_handle

getWikiNewCommentsR :: Text -> Handler Html
getWikiNewCommentsR project_handle = do
    Entity viewer_id viewer <- requireAuth

    maybe_from <- fmap (Key . PersistInt64 . read . T.unpack) <$> lookupGetParam "from"

    now <- liftIO getCurrentTime

    Entity project_id project <- runDB $ getBy404 $ UniqueProjectHandle project_handle

    tags <- runDB $ select $ from $ return

    let tag_map = M.fromList $ entityPairs tags

    (comments, pages, users, retraction_map) <- runDB $ do
        unfiltered_pages <- select $ from $ \ page -> do
            where_ $ page ^. WikiPageProject ==. val project_id
            return page

        let pages = M.fromList $ map (entityKey &&& id) $ {- TODO filter ((userRole viewer >=) . wikiPageCanViewMeta . entityVal) -} unfiltered_pages

        comments <- select $ from $ \ comment -> do
            where_ $ case maybe_from of
                Nothing -> ( comment ^. CommentPage `in_` valList (M.keys pages) )
                Just from_comment -> ( comment ^. CommentPage `in_` valList (M.keys pages)
                                &&. comment ^. CommentId <=. val from_comment )
            orderBy [ desc (comment ^. CommentId) ]
            limit 50
            return comment

        let user_ids = S.toList $ S.fromList $ map (commentUser . entityVal) comments
        users <- fmap (M.fromList . map (entityKey &&& id)) $ select $ from $ \ user -> do
            where_ ( user ^. UserId `in_` valList user_ids )
            return user

        retraction_map <- do
            retractions <- select $ from $ \ comment_retraction -> do
                where_ ( comment_retraction ^. CommentRetractionComment `in_` valList (map entityKey comments) )
                return comment_retraction

            return . M.fromList . map ((commentRetractionComment &&& id) . entityVal) $ retractions

        return (comments, pages, users, retraction_map)

    let PersistInt64 to = unKey $ minimum (map entityKey comments)
        rendered_comments =
            if null comments
             then [whamlet|no new comments|]
             else forM_ comments $ \ (Entity comment_id comment) -> do
                earlier_retractions <- handlerToWidget $ runDB $ do
                    ancestors <- select $ from $ \ comment_ancestor -> do
                        where_ ( comment_ancestor ^. CommentAncestorComment ==. val comment_id )
                        return comment_ancestor

                    fmap (map entityVal) $ select $ from $ \ comment_retraction -> do
                        where_ ( comment_retraction ^. CommentRetractionComment `in_` valList (map (commentAncestorAncestor . entityVal) ancestors))
                        orderBy [ asc (comment_retraction ^. CommentRetractionComment) ]
                        return comment_retraction

                let target = wikiPageTarget $ entityVal $ pages M.! commentPage comment
                    rendered_comment = renderComment viewer_id project_handle target users 1 0 earlier_retractions retraction_map True tag_map (Node (Entity comment_id comment) []) Nothing

                [whamlet|$newline never
                    <div .row>
                        <div .col-md-9>
                            On #
                            <a href="@{WikiR project_handle target}">
                                #{target}
                            :
                            ^{rendered_comment}
                |]

    runDB $ update $ \ user -> do
        set user [ UserReadComments =. val now ]
        where_ ( user ^. UserId ==. val viewer_id )

    defaultLayout $(widgetFile "wiki_new_comments")


getOldWikiHistoryR :: Text -> Text -> Handler Html
getOldWikiHistoryR project_handle target = redirect $ WikiHistoryR project_handle target

getWikiHistoryR :: Text -> Text -> Handler Html
getWikiHistoryR project_handle target = do
    _ <- requireAuthId
    (edits, users) <- runDB $ do
        Entity project_id _ <- getBy404 $ UniqueProjectHandle project_handle
        Entity page_id _ <- getBy404 $ UniqueWikiTarget project_id target
        edits <- select $ from $ \ edit -> do
            where_ ( edit ^. WikiEditPage ==. val page_id )
            orderBy [ desc (edit ^. WikiEditId) ]
            return edit

        let user_id_list = S.toList $ S.fromList $ map (wikiEditUser . entityVal) edits

        users <- fmap (M.fromList . map (entityKey &&& id)) $ select $ from $ \ user -> do
            where_ ( user ^. UserId `in_` valList user_id_list )
            return user

        return (edits, users)

    let editsIndexed = zip ([0..] :: [Int]) edits
    defaultLayout $(widgetFile "wiki_history")


getOldWikiDiffProxyR :: Text -> Text -> Handler Html
getOldWikiDiffProxyR project_handle target = redirect $ WikiDiffProxyR project_handle target

-- | A proxy handler that redirects "ugly" to "pretty" diff URLs,
-- e.g. /w/diff?from=a&to=b to /w/diff/a/b
getWikiDiffProxyR :: Text -> Text -> Handler Html
getWikiDiffProxyR project_handle target = do
    _ <- requireAuthId

    (start_edit_id_t, end_edit_id_t) <- runInputGet $ (,)
                                        <$> ireq textField "start"
                                        <*> ireq textField "end"
    let pairMay = do
        s <- fromPathPiece start_edit_id_t
        e <- fromPathPiece end_edit_id_t
        return (s, e)
    maybe
        (invalidArgs ["revision IDs"])
        (\(s, e) -> redirect $ WikiDiffR project_handle target s e)
        pairMay

getOldWikiDiffR :: Text -> Text -> WikiEditId -> WikiEditId -> Handler Html
getOldWikiDiffR project_handle target start_edit_id end_edit_id = redirect $ WikiDiffR project_handle target start_edit_id end_edit_id

getWikiDiffR :: Text -> Text -> WikiEditId -> WikiEditId -> Handler Html
getWikiDiffR project_handle target start_edit_id end_edit_id = do
    _ <- requireAuthId

    (start_edit, end_edit) <- runDB $ do
        Entity project_id _ <- getBy404 $ UniqueProjectHandle project_handle
        Entity page_id _ <- getBy404 $ UniqueWikiTarget project_id target
        start_edit <- get404 start_edit_id
        end_edit <- get404 end_edit_id

        when (page_id /= wikiEditPage start_edit) $ error "selected 'start' edit is not an edit of selected page"
        when (page_id /= wikiEditPage end_edit) $ error "selected 'end' edit is not an edit of selected page"

        return (start_edit, end_edit)

    let diffEdits = getDiff `on` ((\ (Markdown text) -> T.lines text) . wikiEditContent)
        renderDiff = mconcat . map (\ a -> (case a of Both x _ -> toHtml x; First x -> del (toHtml x); Second x -> ins (toHtml x)) >> br)

    defaultLayout $(widgetFile "wiki_diff")


getOldWikiEditR :: Text -> Text -> WikiEditId -> Handler Html
getOldWikiEditR project_handle target edit_id = redirect $ WikiEditR project_handle target edit_id

getWikiEditR :: Text -> Text -> WikiEditId -> Handler Html
getWikiEditR project_handle target edit_id = do
    _ <- requireAuthId

    edit <- runDB $ do
        Entity project_id _ <- getBy404 $ UniqueProjectHandle project_handle
        Entity page_id _ <- getBy404 $ UniqueWikiTarget project_id target
        edit <- get404 edit_id

        when (page_id /= wikiEditPage edit) $ error "selected edit is not an edit of selected page"

        return edit

    defaultLayout $(widgetFile "wiki_edit")


getOldWikiNewEditsR :: Text -> Handler Html
getOldWikiNewEditsR project_handle = redirect $ WikiNewEditsR project_handle

getWikiNewEditsR :: Text -> Handler Html
getWikiNewEditsR project_handle = do
    Entity viewer_id viewer <- requireAuth

    maybe_from <- fmap (Key . PersistInt64 . read . T.unpack) <$> lookupGetParam "from"

    now <- liftIO getCurrentTime

    Entity project_id project <- runDB $ getBy404 $ UniqueProjectHandle project_handle

    (edits, pages, users) :: ([Entity WikiEdit], M.Map WikiPageId (Entity WikiPage), M.Map UserId (Entity User)) <- runDB $ do
        pages <- fmap (M.fromList . map (entityKey &&& id)) $ select $ from $ \ page -> do
            where_ $ page ^. WikiPageProject ==. val project_id
            return page

        edits <- select $ from $ \ edit -> do
            where_ $ case maybe_from of
                Nothing -> ( edit ^. WikiEditPage `in_` valList (M.keys pages) )
                Just from_edit -> ( edit ^. WikiEditPage `in_` valList (M.keys pages) &&. edit ^. WikiEditId <=. val from_edit )
            orderBy [ desc (edit ^. WikiEditId) ]
            limit 50
            return edit

        let user_ids = S.toList $ S.fromList $ map (wikiEditUser . entityVal) edits
        users <- fmap (M.fromList . map (entityKey &&& id)) $ selectList [ UserId <-. user_ids ] []
        return (edits, pages, users)

    let PersistInt64 to = unKey $ minimum (map entityKey edits)
        renderEdit (Entity edit_id edit) =
            let editor = users M.! wikiEditUser edit
                page = pages M.! wikiEditPage edit
             in [whamlet|
                    <tr>
                        <td>
                            <a href="@{WikiEditR project_handle (wikiPageTarget (entityVal page)) edit_id}">
                                #{wikiPageTarget (entityVal page)}

                        <td>
                            ^{renderTime (wikiEditTs edit)}

                        <td>
                            <a href="@{UserR (entityKey editor)}">
                                #{userPrintName editor}
                        $maybe comment <- wikiEditComment edit
                            <td>
                                #{comment}
                |]

    runDB $ update $ \ user -> do
        set user [ UserReadEdits =. val now ]
        where_ ( user ^. UserId ==. val viewer_id )

    defaultLayout $(widgetFile "wiki_new_edits")


getOldRetractCommentR :: Text -> Text -> CommentId -> Handler Html
getOldRetractCommentR project_handle target comment_id = redirect $ RetractCommentR project_handle target comment_id

getRetractCommentR :: Text -> Text -> CommentId -> Handler Html
getRetractCommentR project_handle target comment_id = do
    Entity user_id user <- requireAuth
    comment <- runDB $ get404 comment_id
    when (commentUser comment /= user_id) $ permissionDenied "You can only retract your own comments."

    earlier_retractions <- runDB $
        case commentParent comment of
            Just parent_id -> do
                ancestors <- do
                    comment_ancestor_entities <- select $ from $ \ comment_ancestor -> do
                        where_ ( comment_ancestor ^. CommentAncestorComment ==. val parent_id )
                        return comment_ancestor

                    return . (parent_id :) . map (commentAncestorAncestor . entityVal) $ comment_ancestor_entities

                fmap (map entityVal) $ select $ from $ \ comment_retraction -> do
                    where_ ( comment_retraction ^. CommentRetractionComment `in_` valList ancestors )
                    return comment_retraction

            Nothing -> return []

    tags <- runDB $ select $ from $ \ (comment_tag `InnerJoin` tag) -> do
        on_ $ comment_tag ^. CommentTagTag ==. tag ^. TagId
        where_ $ comment_tag ^. CommentTagComment ==. val comment_id
        return tag

    let tag_map = M.fromList $ entityPairs tags

    (retract_form, _) <- generateFormPost $ retractForm Nothing

    let rendered_comment = renderDiscussComment user_id project_handle target False (return ()) (Entity comment_id comment) [] (M.singleton user_id $ Entity user_id user) earlier_retractions M.empty False tag_map

    defaultLayout $ [whamlet|
        ^{rendered_comment}
        <form method="POST">
            ^{retract_form}
            <input type="submit" name="mode" value="preview retraction">
    |]


postOldRetractCommentR :: Text -> Text -> CommentId -> Handler Html
postOldRetractCommentR = postRetractCommentR

postRetractCommentR :: Text -> Text -> CommentId -> Handler Html
postRetractCommentR project_handle target comment_id = do
    Entity user_id user <- requireAuth
    comment <- runDB $ get404 comment_id
    when (commentUser comment /= user_id) $ permissionDenied "You can only retract your own comments."

    ((result, _), _) <- runFormPost $ retractForm Nothing

    case result of
        FormSuccess reason -> do
            earlier_retractions <- runDB $
                case commentParent comment of
                    Just parent_id -> do
                        ancestors <- do
                            comment_ancestor_entities <- select $ from $ \ comment_ancestor -> do
                                where_ ( comment_ancestor ^. CommentAncestorComment ==. val parent_id )
                                return comment_ancestor

                            return . (parent_id :) . map (commentAncestorAncestor . entityVal) $ comment_ancestor_entities
                        map entityVal <$> selectList [ CommentRetractionComment <-. ancestors ] []

                    Nothing -> return []

            let action :: Text = "retract"
            mode <- lookupPostParam "mode"
            case mode of
                Just "preview retraction" -> do
                    (form, _) <- generateFormPost $ retractForm (Just reason)

                    tags <- runDB $ select $ from $ \ (comment_tag `InnerJoin` tag) -> do
                        on_ $ comment_tag ^. CommentTagTag ==. tag ^. TagId
                        where_ $ comment_tag ^. CommentTagComment ==. val comment_id
                        return tag

                    let tag_map = M.fromList $ entityPairs tags
                        soon = UTCTime (ModifiedJulianDay 0) 0
                        retraction = CommentRetraction soon reason comment_id
                        comment_entity = Entity comment_id comment
                        users = M.singleton user_id $ Entity user_id user
                        retractions = M.singleton comment_id retraction

                    defaultLayout $ renderPreview form action $ renderDiscussComment user_id project_handle target False (return ()) comment_entity [] users earlier_retractions retractions False tag_map


                Just a | a == action -> do
                    now <- liftIO getCurrentTime
                    _ <- runDB $ insert $ CommentRetraction now reason comment_id

                    redirect $ DiscussCommentR project_handle target comment_id

                _ -> error "Error: unrecognized mode."
        _ -> error "Error when submitting form."



getOldApproveCommentR :: Text -> Text -> CommentId -> Handler Html
getOldApproveCommentR project_handle target comment_id = redirect $ ApproveCommentR project_handle target comment_id

getApproveCommentR :: Text -> Text -> CommentId -> Handler Html
getApproveCommentR project_handle target comment_id = do
    user_id <- requireAuthId

    comment <- runDB $ get404 comment_id
    Entity project_id _ <- runDB $ getBy404 $ UniqueProjectHandle project_handle
    Entity page_id page <- runDB $ getBy404 $ UniqueWikiTarget project_id target

    when (commentPage comment /= page_id) $ error "comment does not match page"
    when (wikiPageProject page /= project_id) $ error "comment does not match project"

    moderator <- runDB $ isProjectModerator project_handle user_id

    when (not moderator) $ error "you must be a moderator to approve posts"

    defaultLayout [whamlet|
        <form method="POST">
            <input type=submit value="approve post">
    |]


postOldApproveCommentR :: Text -> Text -> CommentId -> Handler Html
postOldApproveCommentR = postApproveCommentR

postApproveCommentR :: Text -> Text -> CommentId -> Handler Html
postApproveCommentR project_handle target comment_id = do
    user_id <- requireAuthId

    now <- liftIO getCurrentTime

    comment <- runDB $ get404 comment_id
    Entity project_id _ <- runDB $ getBy404 $ UniqueProjectHandle project_handle
    Entity page_id page <- runDB $ getBy404 $ UniqueWikiTarget project_id target

    when (commentPage comment /= page_id) $ error "comment does not match page"
    when (wikiPageProject page /= project_id) $ error "comment does not match project"

    moderator <- runDB $ isProjectModerator project_handle user_id

    when (not moderator) $ error "you must be a moderator to approve posts"

    runDB $ update $ \ c -> do
        set c
            [ CommentModeratedTs =. val (Just now)
            , CommentModeratedBy =. val (Just user_id)
            ]

        where_ $ c ^. CommentId ==. val comment_id

    addAlert "success" "comment approved"

    redirect $ DiscussCommentR project_handle target comment_id


retractForm :: Maybe Markdown -> Form Markdown
retractForm reason = renderBootstrap3 $ areq snowdriftMarkdownField "Retraction reason:" reason


renderComment :: UserId -> Text -> Text -> M.Map UserId (Entity User) -> Int -> Int
    -> [CommentRetraction] -> M.Map CommentId CommentRetraction -> Bool -> Map TagId Tag -> Tree (Entity Comment) -> Maybe Widget -> Widget

renderComment viewer_id project_handle target users max_depth depth earlier_retractions retraction_map show_actions tag_map tree mcomment_form = do
    maybe_route <- handlerToWidget getCurrentRoute
    (comment_form, _) <- handlerToWidget $ generateFormPost $ commentForm Nothing Nothing

    let Entity comment_id comment = rootLabel tree
        children = subForest tree

        Entity user_id user = users M.! commentUser comment
        author_name = userPrintName (Entity user_id user)
        comment_time = renderTime (commentCreatedTs comment)
        unapproved = not $ isJust $ commentModeratedTs comment

        top_level = (commentDepth comment == 0)
        even_depth = not top_level && commentDepth comment `mod` 2 == 1
        odd_depth = not top_level && not even_depth

        maybe_retraction = M.lookup comment_id retraction_map
        empty_list = []

    tags <- fmap (L.sortBy (compare `on` atName)) $ handlerToWidget $ do
        comment_tags <- runDB $ select $ from $ \ comment_tag -> do
            where_ $ comment_tag ^. CommentTagComment ==. val comment_id
            return comment_tag

        annotateCommentTags tag_map project_handle target comment_id $ map entityVal comment_tags

    $(widgetFile "comment_body")


countReplies :: [Tree a] -> Int
countReplies = sum . map (F.sum . fmap (const 1))


editWikiForm :: WikiEditId -> Markdown -> Maybe Text -> Form (WikiEditId, Markdown, Maybe Text)
editWikiForm last_edit_id content comment = renderBootstrap3 $ (,,)
        <$> areq' hiddenField "" (Just last_edit_id)
        <*> areq' snowdriftMarkdownField "Page Content" (Just content)
        <*> aopt' textField "Comment" (Just comment)


newWikiForm :: Maybe Markdown -> Form Markdown
newWikiForm content = renderBootstrap3 $ areq' snowdriftMarkdownField "Page Content" content


disabledCommentForm :: Form Markdown
disabledCommentForm = renderBootstrap3 $ areq snowdriftMarkdownField ("Reply" { fsAttrs = [("disabled",""), ("class","form-control")] }) Nothing

commentForm :: Maybe CommentId -> Maybe Markdown -> Form (Maybe CommentId, Markdown)
commentForm parent content = renderBootstrap3
    $ (,)
        <$> aopt' hiddenField "" (Just parent)
        <*> areq' snowdriftMarkdownField (if parent == Nothing then "Comment" else "Reply") content

