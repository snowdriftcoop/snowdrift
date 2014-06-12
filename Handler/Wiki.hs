module Handler.Wiki where

import Import

import Widgets.Markdown
import Widgets.Time
import Widgets.Preview

import Model.Permission
import Model.User
import Model.ViewType

import Model.WikiPage

import Yesod.Markdown
import Model.Markdown

import qualified Data.Text as T

import qualified Data.Set as S
import qualified Data.Map as M

import Data.Algorithm.Diff (getDiff, Diff (..))

import Text.Blaze.Html5 (ins, del, br)

getWikiR :: Text -> Text -> Handler Html
getWikiR project_handle target = do
    maybe_user <- maybeAuth

    (Entity _ project, Entity _ page) <- getPageInfo project_handle target

    moderator <- case maybe_user of
        Nothing -> return False
        Just (Entity viewer_id _) ->
            runDB $ isProjectModerator project_handle viewer_id

    [Value (comment_count :: Int)] <- runDB $ select $ from $ \comment -> do
        where_ $ foldl1 (&&.) $ catMaybes
            [ Just $ comment ^. CommentDiscussion ==. val (wikiPageDiscussion page)
            , if moderator then Nothing else Just $ not_ $ isNothing $ comment ^. CommentModeratedTs
            ]
        return countRows

    let can_edit = isJust $ userEstablishedTs =<< entityVal <$> maybe_user

    defaultLayout $ do

        setTitle . toHtml $
            projectName project <> " : " <> wikiPageTarget page <> " | Snowdrift.coop"

        renderWiki comment_count project_handle target can_edit True page


renderWiki :: Int -> Text -> Text -> Bool -> Bool -> WikiPage -> Widget
renderWiki comment_count project_handle target can_edit can_view_meta page = $(widgetFile "wiki")


getWikiPagesR :: Text -> Handler Html
getWikiPagesR project_handle = do
    (Entity _ project) <- runDB $ getBy404 $ UniqueProjectHandle project_handle
    pages <- runDB $ select $ from $ \ (project' `InnerJoin` wiki_page) -> do
        on_ $ project' ^. ProjectId ==. wiki_page ^. WikiPageProject
        where_ $ project' ^. ProjectHandle ==. val project_handle
        orderBy [asc $ wiki_page ^. WikiPageTarget]
        return wiki_page

    defaultLayout $ do
        setTitle . toHtml $ projectName project <> " Wiki | Snowdrift.coop"
        $(widgetFile "wiki_pages")


postWikiR :: Text -> Text -> Handler Html
postWikiR project_handle target = do
    Entity user_id user <- requireAuth
    now <- liftIO getCurrentTime

    let can_edit = isJust $ userEstablishedTs user

    unless can_edit $ permissionDenied "you do not have permission to edit this page"

    (Entity project_id _, Entity page_id page) <- getPageInfo project_handle target

    Entity _ last_edit <- runDB $ getBy404 $ UniqueWikiLastEdit page_id

    ((result, _), _) <- runFormPost $ editWikiForm (wikiLastEditEdit last_edit) (wikiPageContent page) Nothing


    case result of
        FormSuccess (last_edit_id, content, comment) -> do
            mode <- lookupPostParam "mode"

            let action :: Text = "update"

            case mode of
                Just "preview" -> do
                    (form, _) <- generateFormPost $ editWikiForm last_edit_id content (Just comment)

                    defaultLayout $ renderPreview form action $
                        renderWiki 0 project_handle target False False $
                            WikiPage target project_id content (Key $ PersistInt64 (-1)) Normal

                Just x | x == action -> do
                    runDB $ do
                        update $ \ p -> do
                            set p [WikiPageContent =. val content]
                            where_ $ p ^. WikiPageId ==. val page_id

                        edit_id <- insert $ WikiEdit now user_id page_id content (Just comment)
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
                                    , "[original version](" <> target <> "/h/" <> toPathPiece last_edit_id <> ")"
                                    , ""
                                    , "[my version](" <> target <> "/h/" <> toPathPiece edit_id <> ")"
                                    , ""
                                    , "[their version](" <> target <> "/h/" <> toPathPiece (wikiLastEditEdit last_edit) <> ")"
                                    , ""
                                    , "(this ticket was automatically generated)"
                                    ]

                            comment_id <- insert $ Comment now (Just now) (Just user_id) Nothing (wikiPageDiscussion page) Nothing user_id comment_body 0

                            void $ insert $ Ticket now now "edit conflict" comment_id

                            render <- lift getUrlRenderParams
                            let message_text = Markdown $ T.unlines
                                    [ "Edit conflict for wiki page *" <> target <> "*."
                                    , "<br>[**Ticket created**](" <> render (DiscussCommentR project_handle target comment_id) [] <> ")"
                                    ]

                            void $ insert $ Message (Just project_id) now (Just last_editor) (Just user_id) message_text True
                            void $ insert $ Message (Just project_id) now (Just user_id) (Just last_editor) message_text True

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
editWikiPermissionsForm level = renderBootstrap3 $ areq permissionLevelField "Permission Level" (Just level)


getEditWikiPermissionsR :: Text -> Text -> Handler Html
getEditWikiPermissionsR project_handle target = do
    Entity user_id user <- requireAuth
    (Entity project_id project, Entity page_id page) <- getPageInfo project_handle target

    affiliated <- runDB $ (||)
            <$> isProjectAdmin project_handle user_id
            <*> isProjectAdmin "snowdrift" user_id

    unless affiliated $ permissionDenied "you do not have permission to edit page permissions"

    (wiki_form, _) <- generateFormPost $ editWikiPermissionsForm (wikiPagePermissionLevel page)

    defaultLayout $ do
        setTitle . toHtml $ projectName project <> " Wiki Permissions - " <> target <> " | Snowdrift.coop"
        $(widgetFile "edit_wiki_perm")


postEditWikiPermissionsR :: Text -> Text -> Handler Html
postEditWikiPermissionsR project_handle target = do
    Entity user_id _ <- requireAuth
    (_, Entity page_id page) <- getPageInfo project_handle target

    affiliated <- runDB $ (||)
            <$> isProjectAdmin project_handle user_id
            <*> isProjectAdmin "snowdrift" user_id

    unless affiliated $ permissionDenied "you do not have permission to edit page permissions"

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


getEditWikiR :: Text -> Text -> Handler Html
getEditWikiR project_handle target = do
    Entity user_id user <- requireAuth
    (Entity project_id project, Entity page_id page) <- getPageInfo project_handle target

    Entity _ last_edit <- runDB $ getBy404 $ UniqueWikiLastEdit page_id

    let can_edit = isJust $ userEstablishedTs user

    unless can_edit $ permissionDenied "you do not have permission to edit this page"

    (wiki_form, _) <- generateFormPost $ editWikiForm (wikiLastEditEdit last_edit) (wikiPageContent page) Nothing

    defaultLayout $ do
        setTitle . toHtml $ projectName project <> " Wiki - " <> wikiPageTarget page <> " | Snowdrift.coop"
        $(widgetFile "edit_wiki")


getNewWikiR :: Text -> Text -> Handler Html
getNewWikiR project_handle target = do
    Entity user_id user <- requireAuth
    Entity _ project <- runDB $ getBy404 $ UniqueProjectHandle project_handle
    affiliated <- runDB $ (||)
            <$> isProjectAffiliated project_handle user_id
            <*> isProjectAdmin "snowdrift" user_id

    unless affiliated $ permissionDenied "you do not have permission to edit this page"

    (wiki_form, _) <- generateFormPost $ newWikiForm Nothing

    defaultLayout $ do
        setTitle . toHtml $ projectName project <> " Wiki - New Page | Snowdrift.coop"
        $(widgetFile "new_wiki")


postNewWikiR :: Text -> Text -> Handler Html
postNewWikiR project_handle target = do
    Entity user_id _ <- requireAuth

    affiliated <- runDB $ (||)
            <$> isProjectAffiliated project_handle user_id
            <*> isProjectAdmin "snowdrift" user_id

    unless affiliated $ permissionDenied "you do not have permission to edit this page"

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
                        defaultLayout $ renderPreview form action $ renderWiki 0 project_handle target False False page
                            where page = WikiPage target project_id content (Key $ PersistInt64 0) Normal


                Just x | x == action -> do
                    _ <- runDB $ do
                        discussion <- insert (Discussion 0)
                        page_id <- insert $ WikiPage target project_id content discussion Normal
                        edit_id <- insert $ WikiEdit now user_id page_id content $ Just "Page created."
                        insert $ WikiLastEdit page_id edit_id

                    addAlert "success" "Created."
                    redirect $ WikiR project_handle target

                _ -> error "unrecognized mode"

        FormMissing -> error "Form missing."
        FormFailure msgs -> error $ "Error submitting form: " ++ T.unpack (T.concat msgs)


getWikiHistoryR :: Text -> Text -> Handler Html
getWikiHistoryR project_handle target = do
    --_ <- requireAuthId
    (Entity project_id project, Entity page_id _) <- getPageInfo project_handle target

    (edits, users) <- runDB $ do
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
    defaultLayout $ do
        setTitle . toHtml $ projectName project <> " Wiki History - " <> target <> " | Snowdrift.coop"
        $(widgetFile "wiki_history")


-- | A proxy handler that redirects "ugly" to "pretty" diff URLs,
-- e.g. /w/diff?from=a&to=b to /w/diff/a/b
getWikiDiffProxyR :: Text -> Text -> Handler Html
getWikiDiffProxyR project_handle target = do
--    _ <- requireAuthId

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


getWikiDiffR :: Text -> Text -> WikiEditId -> WikiEditId -> Handler Html
getWikiDiffR project_handle target start_edit_id end_edit_id = do
--    _ <- requireAuthId

    (Entity project_id project, Entity page_id _) <- getPageInfo project_handle target

    (start_edit, end_edit) <- runDB $ do
        start_edit <- get404 start_edit_id
        end_edit <- get404 end_edit_id

        when (page_id /= wikiEditPage start_edit) $ error "selected 'start' edit is not an edit of selected page"
        when (page_id /= wikiEditPage end_edit) $ error "selected 'end' edit is not an edit of selected page"

        return (start_edit, end_edit)

    let diffEdits = getDiff `on` ((\ (Markdown text) -> T.lines text) . wikiEditContent)
        renderDiff = mconcat . map (\ a -> (case a of Both x _ -> toHtml x; First x -> del (toHtml x); Second x -> ins (toHtml x)) >> br)

    defaultLayout $ do
        setTitle . toHtml $ projectName project <> " Wiki Diff - " <> target <> " | Snowdrift.coop"
        $(widgetFile "wiki_diff")


-- This handles any links we might have to the old /history/# style links
-- just in case any exist. We could remove it if we're willing to let
-- something break or can check that there's no such links
-- (it's unlikely there's any at all, certainly if so they are
-- almost certainly internal anyway)
getOldWikiEditR :: Text -> Text -> WikiEditId -> Handler Html
getOldWikiEditR project_handle target edit_id = redirect $ WikiEditR project_handle target edit_id


getWikiEditR :: Text -> Text -> WikiEditId -> Handler Html
getWikiEditR project_handle target edit_id = do
--    _ <- requireAuthId

    (Entity project_id project, Entity page_id _) <- getPageInfo project_handle target
    edit <- runDB $ do
        edit <- get404 edit_id

        when (page_id /= wikiEditPage edit) $ error "selected edit is not an edit of selected page"

        return edit

    defaultLayout $ do
    -- TODO: prettier date format? or edit id?
        setTitle . toHtml $ projectName project <> " Wiki - " <> target <> " at " <> T.pack (show $ wikiEditTs edit) <> " | Snowdrift.coop"
        $(widgetFile "wiki_edit")


getWikiNewEditsR :: Text -> Handler Html
getWikiNewEditsR project_handle = do
    mauth <- maybeAuth
    Entity project_id project <- runDB $ getBy404 $ UniqueProjectHandle project_handle

    maybe_from <- fmap (Key . PersistInt64 . read . T.unpack) <$> lookupGetParam "from"

    req <- getRequest
    maybe_since <- lookupGetParam "since"
    since :: UTCTime <- case mauth of
        Nothing -> liftIO getCurrentTime
        Just (Entity viewer_id viewer) -> case maybe_since of
            Nothing -> do
                viewtimes :: [Entity ViewTime] <- runDB $ select $ from $ \ viewtime -> do
                        where_ $
                            ( viewtime ^. ViewTimeUser ==. val viewer_id ) &&.
                            ( viewtime ^. ViewTimeProject ==. val project_id ) &&.
                            ( viewtime ^. ViewTimeType ==. val ViewEdits )
                        return viewtime

                let comments_ts = case viewtimes of
                        [] -> userReadEdits viewer
                        Entity _ viewtime : _ -> viewTimeTime viewtime

                redirectParams (WikiNewEditsR project_handle) $ (T.pack "since", T.pack $ show comments_ts) : reqGetParams req

            Just since -> return (read . T.unpack $ since)

    now <- liftIO getCurrentTime


    (new_edits, old_edits, pages, users) :: ([Entity WikiEdit], [Entity WikiEdit], M.Map WikiPageId (Entity WikiPage), M.Map UserId (Entity User)) <- runDB $ do
        pages <- fmap (M.fromList . map (entityKey &&& id)) $ select $ from $ \ page -> do
            where_ $ page ^. WikiPageProject ==. val project_id
            return page

{-
        viewtimes :: [Entity ViewTime] <- select $ from $ \ viewtime -> do
            where_ $
                ( viewtime ^. ViewTimeUser ==. val viewer_id ) &&.
                ( viewtime ^. ViewTimeProject ==. val project_id ) &&.
                ( viewtime ^. ViewTimeType ==. val ViewEdits )
            return viewtime

        let edits_ts = case viewtimes of
                [] -> userReadEdits viewer
                (Entity _ viewtime):_ -> viewTimeTime viewtime
-}
        new_edits :: [Entity WikiEdit] <- select $ from $ \ edit -> do
            where_ $
                case maybe_from of
                    Nothing ->
                        ( edit ^. WikiEditPage `in_` valList (M.keys pages) ) &&.
                        ( edit ^. WikiEditTs >=. val since )
                    Just from_edit ->
                        ( edit ^. WikiEditPage `in_` valList (M.keys pages) ) &&.
                        ( edit ^. WikiEditId <=. val from_edit ) &&.
                        ( edit ^. WikiEditTs >=. val since )

            orderBy [ desc (edit ^. WikiEditId) ]
            limit 51
            return edit

        old_edits :: [Entity WikiEdit] <- select $ from $ \ edit -> do
            where_ $
                case maybe_from of
                    Nothing ->
                        ( edit ^. WikiEditPage `in_` valList ( M.keys pages) ) &&.
                        ( edit ^. WikiEditTs <. val since )
                    Just from_edit ->
                        ( edit ^. WikiEditPage `in_` valList (M.keys pages) ) &&.
                        ( edit ^. WikiEditId <=. val from_edit ) &&.
                        ( edit ^. WikiEditTs <. val since )
            orderBy [ desc (edit ^. WikiEditId) ]
            limit $ fromIntegral $ 51 - length new_edits
            --offset $ fromIntegral $ length new_edits
            return edit

        let user_ids = S.toList $ S.fromList $ map (wikiEditUser . entityVal) (new_edits <> old_edits)
        users <- fmap (M.fromList . map (entityKey &&& id)) $ selectList [ UserId <-. user_ids ] []
        return (new_edits, old_edits, pages, users)

    let new_edits' = take 50 new_edits
        old_edits' = take (50 - length new_edits') old_edits
        show_older = (length new_edits + length old_edits) > 50
        PersistInt64 to = unKey $ minimum (map entityKey (new_edits' <> old_edits'))
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
                        <td>
                            $maybe comment <- wikiEditComment edit
                                #{comment}
                |]

    case mauth of
        Nothing -> return ()
        Just (Entity viewer_id _) -> runDB $ do
            c <- updateCount $ \ viewtime -> do
                    set viewtime [ ViewTimeTime =. val now ]
                    where_ $
                        ( viewtime ^. ViewTimeUser ==. val viewer_id ) &&.
                        ( viewtime ^. ViewTimeProject ==. val project_id ) &&.
                        ( viewtime ^. ViewTimeType ==. val ViewEdits )

            when (c == 0) $ insert_ $ ViewTime viewer_id project_id ViewEdits now

    defaultLayout $ do
        setTitle . toHtml $ projectName project <> " - New Wiki Edits | Snowdrift.coop"
        $(widgetFile "wiki_new_edits")


editWikiForm :: WikiEditId -> Markdown -> Maybe Text -> Form (WikiEditId, Markdown, Text)
editWikiForm last_edit_id content comment = renderBootstrap3 $ (,,)
        <$> areq' hiddenField "" (Just last_edit_id)
        <*> areq' snowdriftMarkdownField "Page Content" (Just content)
        <*> areq' textField "Comment" comment


newWikiForm :: Maybe Markdown -> Form Markdown
newWikiForm content = renderBootstrap3 $ areq' snowdriftMarkdownField "Page Content" content


