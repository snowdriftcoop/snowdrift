-- | Handler for Wiki paths. Section comments are relative to /p/#handle/w

module Handler.Wiki where

import Import

import qualified Data.Tree.Extra      as Tree
import           Data.Tree.Extra      (sortForestBy)
import           Handler.Wiki.Comment (getMaxDepth, processWikiComment)
import           Model.Comment
import           Model.Markdown
import           Model.Message
import           Model.Permission
import           Model.Project
import           Model.Tag            (getAllTagsMap)
import           Model.User
import           Model.ViewTime
import           Model.ViewType
import           Model.WikiPage
import           Widgets.Preview
import           Widgets.Time
import           View.Comment
import           View.Wiki

import           Data.Algorithm.Diff  (getDiff, Diff (..))
import qualified Data.Map             as M
import qualified Data.Set             as S
import qualified Data.Text            as T
import           Text.Blaze.Html5     (ins, del, br)
import           Text.Cassius         (cassiusFile)
import           Yesod.Markdown

--------------------------------------------------------------------------------
-- Utility functions

getPageInfo :: Text -> Text -> YDB (Entity Project, Entity WikiPage)
getPageInfo project_handle target = do
    project <- getBy404 $ UniqueProjectHandle project_handle
    page    <- getBy404 $ UniqueWikiTarget (entityKey project) target
    return (project, page)

--------------------------------------------------------------------------------
-- /

getWikiPagesR :: Text -> Handler Html
getWikiPagesR project_handle = do
    (project, pages) <- runYDB $ do
        Entity project_id project <- getBy404 $ UniqueProjectHandle project_handle
        pages <- getProjectWikiPages project_id
        return (project, pages)
    defaultLayout $ do
        setTitle . toHtml $ projectName project <> " Wiki | Snowdrift.coop"
        $(widgetFile "wiki_pages")

-- | Redirect 'here' with added GET parameters.
redirectHereWithParams :: [(Text, Text)] -> Handler a
redirectHereWithParams new_params = do
    Just route <- getCurrentRoute
    getRequest >>= redirectParams route . (new_params ++) . reqGetParams

--------------------------------------------------------------------------------
-- /newcomments

getWikiNewCommentsR :: Text -> Handler Html
getWikiNewCommentsR project_handle = do
    mviewer <- maybeAuth
    Entity project_id project <- runYDB $ getBy404 $ UniqueProjectHandle project_handle

    now <- liftIO getCurrentTime

    since :: UTCTime <- case mviewer of
        Nothing -> return now
        Just (Entity viewer_id viewer) -> lookupGetParam "since" >>= \case
            Nothing -> do
                comments_ts <- maybe (userReadComments viewer) (viewTimeTime . entityVal) <$>
                                   runDB (getCommentsViewTime viewer_id project_id)
                redirectHereWithParams [("since", T.pack $ show comments_ts)]
            Just since -> return (read . T.unpack $ since)

    latest_comment_id <- Key . PersistInt64 . fromMaybe maxBound <$> runInputGet (iopt intField "latest")

    (unapproved_comments, new_comments, old_comments, users, closure_map, ticket_map, flag_map, tag_map) <- runDB $ do
        (unapproved_comments, new_comments, old_comments) <-
            getAllWikiComments (entityKey <$> mviewer) project_id latest_comment_id since 51

        let all_comment_entities = unapproved_comments <> new_comments <> old_comments
            all_comments         = map entityVal all_comment_entities
            all_comment_ids      = map entityKey all_comment_entities

        users <- entitiesMap <$> getUsersIn (S.toList . S.fromList $ map commentUser all_comments)

        closure_map <- makeClosureMap all_comment_ids
        ticket_map  <- makeTicketMap  all_comment_ids
        flag_map    <- makeFlagMap    all_comment_ids
        tag_map     <- getAllTagsMap

        return (unapproved_comments, new_comments, old_comments, users, closure_map, ticket_map, flag_map, tag_map)

    let new_comments' = take 50 new_comments
        old_comments' = take (50 - length new_comments') old_comments
        PersistInt64 to = unKey $ minimum (map entityKey (new_comments' <> old_comments') )
        render_comments comments =
            if null comments
                then [whamlet||]
                -- TODO(mitchell): This code could use makeCommentWidget, and we wouldn't need
                -- many queries above (users, closure_map, etc), but many more database hits
                -- would result (one per new comment). Meh...
                else do
                    forM_ comments $ \ (Entity comment_id comment) -> do
                        (earlier_closures, target) <- handlerToWidget $ runDB $ (,)
                            <$> getAncestorClosures comment_id
                            <*> (wikiPageTarget <$> getCommentPage comment_id)

                        let rendered_comment =
                                commentTreeWidget
                                    mempty
                                    (Tree.singleton (Entity comment_id comment))
                                    earlier_closures
                                    users
                                    closure_map
                                    ticket_map
                                    flag_map
                                    tag_map
                                    project_handle
                                    target
                                    True   -- show actions?
                                    0      -- max_depth is irrelevant for the new-comments listing 0
                                    0

                        [whamlet|$newline never
                            <div .row>
                                <div .col-md-9 .col-md-offset-1 .col-lg-8 .col-lg-offset-2>
                                    <h4>
                                        On #
                                        <a href="@{WikiR project_handle target}">
                                            #{target}
                                        :
                                    ^{rendered_comment}
                        |]
                    toWidget $(cassiusFile "templates/comment_wrapper.cassius")

        rendered_unapproved_comments = render_comments unapproved_comments
        rendered_new_comments        = render_comments new_comments'
        rendered_old_comments        = render_comments old_comments'
        show_older = (length new_comments + length old_comments) > 50

    case mviewer of
        Nothing -> return ()
        Just (Entity viewer_id _) -> runDB $ updateCommentsViewTime now viewer_id project_id

    defaultLayout $ do
        setTitle . toHtml $ projectName project <> " - New Comments | Snowdrift.coop"
        $(widgetFile "wiki_new_comments")

--------------------------------------------------------------------------------
-- /newedits

getWikiNewEditsR :: Text -> Handler Html
getWikiNewEditsR project_handle = do
    mauth <- maybeAuth
    Entity project_id project <- runYDB $ getBy404 $ UniqueProjectHandle project_handle

    maybe_from <- fmap (Key . PersistInt64 . read . T.unpack) <$> lookupGetParam "from"

    since :: UTCTime <- case mauth of
        Nothing -> liftIO getCurrentTime
        Just (Entity viewer_id viewer) -> lookupGetParam "since" >>= \case
            Nothing -> do
                viewtimes :: [Entity ViewTime] <- runDB $
                    select $
                    from $ \ viewtime -> do
                    where_ $
                        viewtime ^. ViewTimeUser ==. val viewer_id &&.
                        viewtime ^. ViewTimeProject ==. val project_id &&.
                        viewtime ^. ViewTimeType ==. val ViewEdits
                    return viewtime

                let comments_ts = case viewtimes of
                        [] -> userReadEdits viewer
                        Entity _ viewtime : _ -> viewTimeTime viewtime

                redirectHereWithParams [("since", T.pack $ show comments_ts)]

            Just since -> return (read . T.unpack $ since)

    now <- liftIO getCurrentTime


    (new_edits, old_edits, pages, users) :: ([Entity WikiEdit], [Entity WikiEdit], M.Map WikiPageId (Entity WikiPage), M.Map UserId (Entity User)) <- runDB $ do
        pages <- fmap (M.fromList . map (entityKey &&& id)) $
            select $
            from $ \ page -> do
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
                     viewtime ^. ViewTimeUser ==. val viewer_id &&.
                     viewtime ^. ViewTimeProject ==. val project_id &&.
                     viewtime ^. ViewTimeType ==. val ViewEdits

            when (c == 0) $ insert_ $ ViewTime viewer_id project_id ViewEdits now

    defaultLayout $ do
        setTitle . toHtml $ projectName project <> " - New Wiki Edits | Snowdrift.coop"
        $(widgetFile "wiki_new_edits")

--------------------------------------------------------------------------------
-- /#target

getWikiR :: Text -> Text -> Handler Html
getWikiR project_handle target = do
    maybe_user <- maybeAuth
    (Entity project_id project, Entity _ page) <- runYDB $ getPageInfo project_handle target

    comment_count <- runDB $ do
        let muser_id      = entityKey <$> maybe_user
            discussion_id = wikiPageDiscussion page
        roots_ids <- map entityKey <$> getAllOpenRootComments muser_id project_id discussion_id
        children <- getCommentsDescendants muser_id project_id roots_ids
        return $ length roots_ids + length children

    let can_edit = fromMaybe False (isEstablished . entityVal <$> maybe_user)

    defaultLayout $ do

        setTitle . toHtml $
            projectName project <> " : " <> wikiPageTarget page <> " | Snowdrift.coop"

        renderWiki comment_count project_handle target can_edit True page

postWikiR :: Text -> Text -> Handler Html
postWikiR project_handle target = do
    Entity user_id user <- requireAuth
    now <- liftIO getCurrentTime

    let can_edit = isEstablished user

    unless can_edit $ permissionDenied "you do not have permission to edit this page"

    (Entity project_id _, Entity page_id page) <- runYDB $ getPageInfo project_handle target

    Entity _ last_edit <- runYDB $ getBy404 $ UniqueWikiLastEdit page_id

    ((result, _), _) <- runFormPost $ editWikiForm (wikiLastEditEdit last_edit) (wikiPageContent page) Nothing


    case result of
        FormSuccess (last_edit_id, content, comment) -> do
            mode <- lookupPostParam "mode"

            let action :: Text = "update"

            case mode of
                Just "preview" -> do
                    (form, _) <- generateFormPost $ editWikiForm last_edit_id content (Just comment)

                    defaultLayout $ previewWidget form action $
                        renderWiki 0 project_handle target False False $
                            WikiPage target project_id content (Key $ PersistInt64 (-1)) Normal

                Just x | x == action -> do
                    runSYDB $ do
                        lift $
                            update $ \ p -> do
                            set p [WikiPageContent =. val content]
                            where_ $ p ^. WikiPageId ==. val page_id

                        edit_id <- lift $ insert $ WikiEdit now user_id page_id content (Just comment)
                        -- TODO - I think there might be a race condition here...
                        either_last_edit <- lift $ insertBy $ WikiLastEdit page_id edit_id

                        if last_edit_id == wikiLastEditEdit last_edit
                         then lift $ lift $ addAlert "success" "Updated."
                         else do
                            [ Value last_editor ] <- lift $
                                select $
                                from $ \edit -> do
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

                            comment_id <- lift $ insert =<< makeModeratedComment user_id (wikiPageDiscussion page) Nothing comment_body 0

                            lift $ insert_ $ Ticket now now "edit conflict" comment_id

                            render <- lift getUrlRenderParams
                            let message_text = Markdown $ T.unlines
                                    [ "Edit conflict for wiki page *" <> target <> "*."
                                    , "<br>[**Ticket created**](" <> render (DiscussCommentR project_handle target comment_id) [] <> ")"
                                    ]

                            insertMessage_ $ Message MessageDirect (Just project_id) now (Just last_editor) (Just user_id) message_text True
                            insertMessage_ $ Message MessageDirect (Just project_id) now (Just user_id) (Just last_editor) message_text True

                            lift $ lift $ addAlert "danger" "conflicting edits (ticket created, messages sent)"

                        case either_last_edit of
                            Left (Entity to_update _) -> lift $
                                update $ \l -> do
                                set l [WikiLastEditEdit =. val edit_id]
                                where_ $ l ^. WikiLastEditId ==. val to_update
                            Right _ -> return ()

                    redirect $ WikiR project_handle target

                _ -> error "Error: unrecognized mode"


        FormMissing -> error "Form missing."
        FormFailure msgs -> error $ "Error submitting form: " ++ T.unpack (T.concat msgs)


--------------------------------------------------------------------------------
-- /#target/d

-- | getDiscussWikiR generates the associated discussion page for each wiki page
getDiscussWikiR :: Text -> Text -> Handler Html
getDiscussWikiR project_handle target = lookupGetParam "state" >>= \case
    Just "closed" -> go getAllClosedRootComments
    _             -> go getAllOpenRootComments
  where
    go = getDiscussWikiR' project_handle target

getDiscussWikiR' :: Text                      -- ^ Project handle.
                 -> Text                      -- ^ Wiki page name.
                 -> (Maybe UserId
                     -> ProjectId
                     -> DiscussionId
                     -> DB [Entity Comment])  -- ^ Root comment getter.
                 -> Handler Html
getDiscussWikiR' project_handle target get_root_comments = do
    muser <- maybeAuth
    let muser_id = entityKey <$> muser

    (Entity project_id project, Entity _ page) <- runYDB $ getPageInfo project_handle target

    (roots, replies, user_map, closure_map, ticket_map, flag_map, tag_map) <- runDB $ do
        roots           <- get_root_comments muser_id project_id (wikiPageDiscussion page)
        replies         <- getCommentsDescendants muser_id project_id (map entityKey roots)
        user_map        <- entitiesMap <$> getUsersIn (S.toList $ getCommentsUsers roots <> getCommentsUsers replies)
        let comment_ids  = map entityKey (roots ++ replies)
        closure_map     <- makeClosureMap comment_ids
        ticket_map      <- makeTicketMap  comment_ids
        flag_map        <- makeFlagMap    comment_ids
        tag_map         <- getAllTagsMap
        return (roots, replies, user_map, closure_map, ticket_map, flag_map, tag_map)

    max_depth <- getMaxDepth

    -- TODO(mitchell): use makeCommentWidget here
    let comments = do
            forM_ (sortForestBy orderingNewestFirst (buildCommentForest roots replies)) $ \comment ->
                commentTreeWidget
                    mempty
                    comment
                    [] -- earlier closures
                    user_map
                    closure_map
                    ticket_map
                    flag_map
                    tag_map
                    project_handle
                    target
                    True           -- show actions?
                    max_depth
                    0              -- depth
            toWidget $(cassiusFile "templates/comment_wrapper.cassius")

    (comment_form, _) <- generateFormPost commentNewTopicForm

    let has_comments = not $ null roots

    defaultLayout $ do
        setTitle . toHtml $ projectName project <> " Wiki Discussion - " <> target <> " | Snowdrift.coop"
        $(widgetFile "wiki_discuss")

--------------------------------------------------------------------------------
-- /#target/d/new

getNewDiscussWikiR :: Text -> Text -> Handler Html
getNewDiscussWikiR project_handle target = do
    void requireAuth
    (comment_form, _) <- generateFormPost commentNewTopicForm
    defaultLayout $(widgetFile "wiki_discuss_new")

postNewDiscussWikiR :: Text -> Text -> Handler Html
postNewDiscussWikiR project_handle target = do
    (project_entity, Entity _ page) <- runYDB $ getPageInfo project_handle target

    ((result, _), _) <- runFormPost commentNewTopicForm

    case result of
        FormSuccess text -> do
            mode <- lookupPostParam "mode"
            processWikiComment mode Nothing text project_entity page
        FormMissing      -> error "Form missing."
        FormFailure msgs -> error $ "Error submitting form: " ++ T.unpack (T.intercalate "\n" msgs)

--------------------------------------------------------------------------------
-- /#target/diff/#from/#to
-- /#target/diffp

getWikiDiffR :: Text -> Text -> WikiEditId -> WikiEditId -> Handler Html
getWikiDiffR project_handle target start_edit_id end_edit_id = do
    (Entity _ project, Entity page_id _) <- runYDB $ getPageInfo project_handle target

    (start_edit, end_edit) <- runYDB $ (,)
        <$> get404 start_edit_id
        <*> get404 end_edit_id

    when (page_id /= wikiEditPage start_edit) $ error "selected 'start' edit is not an edit of selected page"
    when (page_id /= wikiEditPage end_edit)   $ error "selected 'end' edit is not an edit of selected page"

    let diffEdits = getDiff `on` ((\ (Markdown text) -> T.lines text) . wikiEditContent)
        renderDiff = mconcat . map (\ a -> (case a of Both x _ -> toHtml x; First x -> del (toHtml x); Second x -> ins (toHtml x)) >> br)

    defaultLayout $ do
        setTitle . toHtml $ projectName project <> " Wiki Diff - " <> target <> " | Snowdrift.coop"
        $(widgetFile "wiki_diff")

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

--------------------------------------------------------------------------------
-- /#target/edit

getEditWikiR :: Text -> Text -> Handler Html
getEditWikiR project_handle target = do
    user <- entityVal <$> requireAuth
    (Entity _ project, Entity page_id page) <- runYDB $ getPageInfo project_handle target

    Entity _ last_edit <- runYDB $ getBy404 $ UniqueWikiLastEdit page_id

    let can_edit = isEstablished user

    unless can_edit $ permissionDenied "you do not have permission to edit this page"

    (wiki_form, _) <- generateFormPost $ editWikiForm (wikiLastEditEdit last_edit) (wikiPageContent page) Nothing

    defaultLayout $ do
        setTitle . toHtml $ projectName project <> " Wiki - " <> wikiPageTarget page <> " | Snowdrift.coop"
        $(widgetFile "edit_wiki")

--------------------------------------------------------------------------------
-- /#target/h

getWikiHistoryR :: Text -> Text -> Handler Html
getWikiHistoryR project_handle target = do
    (Entity _ project, Entity page_id _) <- runYDB $ getPageInfo project_handle target

    (edits, users) <- runDB $ do
        edits <-
            select $
            from $ \ edit -> do
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

--------------------------------------------------------------------------------
-- /#target/h/#edit

getWikiEditR :: Text -> Text -> WikiEditId -> Handler Html
getWikiEditR project_handle target edit_id = do
    (Entity _ project, Entity page_id _) <- runYDB $ getPageInfo project_handle target
    edit <- runYDB $ do
        edit <- get404 edit_id

        when (page_id /= wikiEditPage edit) $ error "selected edit is not an edit of selected page"

        return edit

    defaultLayout $ do
    -- TODO: prettier date format? or edit id?
        setTitle . toHtml $ projectName project <> " Wiki - " <> target <> " at " <> T.pack (show $ wikiEditTs edit) <> " | Snowdrift.coop"
        $(widgetFile "wiki_edit")

--------------------------------------------------------------------------------
-- /#target/new

getNewWikiR :: Text -> Text -> Handler Html
getNewWikiR project_handle target = do
    user_id <- requireAuthId
    Entity _ project <- runYDB $ getBy404 $ UniqueProjectHandle project_handle
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

    unless affiliated $
        permissionDenied "you do not have permission to edit this page"

    now <- liftIO getCurrentTime

    Entity project_id _ <- runYDB . getBy404 $ UniqueProjectHandle project_handle

    ((result, _), _) <- runFormPost $ newWikiForm Nothing

    case result of
        FormSuccess content -> do
            mode <- lookupPostParam "mode"
            let action :: Text = "create"
            case mode of
                Just "preview" -> do
                        (form, _) <- generateFormPost $ newWikiForm (Just content)
                        defaultLayout $ do
                            let page = WikiPage target project_id content (Key $ PersistInt64 0) Normal
                            previewWidget form action $ renderWiki 0 project_handle target False False page

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



--------------------------------------------------------------------------------
-- /#target/perm

getEditWikiPermissionsR :: Text -> Text -> Handler Html
getEditWikiPermissionsR project_handle target = do
    user_id <- requireAuthId
    (Entity _ project, Entity _ page) <- runYDB $ getPageInfo project_handle target

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
    (_, Entity page_id page) <- runYDB $ getPageInfo project_handle target

    affiliated <- runDB $ (||)
            <$> isProjectAdmin project_handle user_id
            <*> isProjectAdmin "snowdrift" user_id

    unless affiliated $ permissionDenied "you do not have permission to edit page permissions"

    ((result, _), _) <- runFormPost $ editWikiPermissionsForm (wikiPagePermissionLevel page)

    case result of
        FormSuccess level -> do
            runDB $
                update $ \ p -> do
                where_ $ p ^. WikiPageId ==. val page_id
                set p [ WikiPagePermissionLevel =. val level ]

            addAlert "success" "permissions updated"

            redirect $ WikiR project_handle target

        FormMissing -> error "Form missing."
        FormFailure msgs -> error $ "Error submitting form: " ++ T.unpack (T.concat msgs)

--------------------------------------------------------------------------------
-- DEPRECATED

-- This handles any links we might have to the old /history/# style links
-- just in case any exist. We could remove it if we're willing to let
-- something break or can check that there's no such links
-- (it's unlikely there's any at all, certainly if so they are
-- almost certainly internal anyway)
getOldWikiEditR :: Text -> Text -> WikiEditId -> Handler Html
getOldWikiEditR project_handle target edit_id = redirect $ WikiEditR project_handle target edit_id


