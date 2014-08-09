-- | Handler for Wiki paths. Section comments are relative to /p/#handle/w

module Handler.Wiki where

import Import

import           Data.Tree.Extra      (sortForestBy)
import           Handler.Wiki.Comment (getMaxDepth, processWikiComment)
import           Model.Comment
import           Model.Markdown
import           Model.Message
import           Model.Permission
import           Model.Tag            (getAllTagsMap)
import           Model.User
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

-- | Get the Project/WikiPage entities.
pageInfo :: Text -> Text -> YDB (Entity Project, Entity WikiPage)
pageInfo project_handle target = do
    project <- getBy404 $ UniqueProjectHandle project_handle
    page    <- getBy404 $ UniqueWikiTarget (entityKey project) target
    return (project, page)

-- | Get the Project/WikiPage entities, but require some generic permissions,
-- failing with permissionDenied if they are not satisfied.
pageInfoRequirePermission :: Text                                                          -- Project handle.
                          -> Text                                                          -- Wiki page.
                          -> (Entity User -> Entity Project -> Entity WikiPage -> DB Bool) -- Permission checker.
                          -> Handler (Entity User, Entity Project, Entity WikiPage)
pageInfoRequirePermission project_handle target has_permission = do
    user <- requireAuth
    (project, page, ok) <- runYDB $ do
        project <- getBy404 (UniqueProjectHandle project_handle)
        page    <- getBy404 (UniqueWikiTarget (entityKey project) target)
        ok <- has_permission user project page
        return (project, page, ok)
    unless ok (permissionDenied "You don't have permission to access this page.")
    return (user, project, page)

-- | Like pageInfoRequirePermission, but specialized to requiring Project affiliation.
pageInfoRequireAffiliation :: Text -> Text -> Handler (Entity User, Entity Project, Entity WikiPage)
pageInfoRequireAffiliation project_handle target =
    pageInfoRequirePermission project_handle target (\(Entity user_id _) (Entity project_id _) _ ->
      userIsAffiliatedWithProjectDB user_id project_id)

-- | Like pageInfoRequireAffiliation, but this is for creating a new WikiPage, so one doesn't
-- already exist. TODO(mitchell): Make a better abstraction here.
pageInfoRequireAffiliation' :: Text -> Handler (Entity User, Entity Project)
pageInfoRequireAffiliation' project_handle = do
    user@(Entity user_id _) <- requireAuth
    (project, ok) <- runYDB $ do
        project@(Entity project_id _) <- getBy404 (UniqueProjectHandle project_handle)
        ok <- userIsAffiliatedWithProjectDB user_id project_id
        return (project, ok)
    unless ok (permissionDenied "You don't have permission to access this page.")
    return (user, project)

-- | Like pageInfoRequirePermission, but specialized to requiring that the User can edit a WikiPage.
pageInfoRequireCanEdit :: Text -> Text -> Handler (Entity User, Entity Project, Entity WikiPage)
pageInfoRequireCanEdit project_handle target =
    pageInfoRequirePermission project_handle target (\(Entity _ user) _ _ -> return (userCanEditWikiPage user))

--------------------------------------------------------------------------------
-- /#target

getWikiR :: Text -> Text -> Handler Html
getWikiR project_handle target = do
    maybe_user <- maybeAuth
    (project, page, comment_count) <- runYDB $ do
        (Entity project_id project, Entity page_id page) <- pageInfo project_handle target

        let muser_id      = entityKey <$> maybe_user
            discussion_id = wikiPageDiscussion page

        case muser_id of
            Nothing -> return ()
            Just user_id -> do
                is_watching <- userIsWatchingProjectDB user_id project_id
                when is_watching $
                    userViewWikiEditsDB user_id page_id

        roots_ids <- map entityKey <$> fetchAllOpenRootCommentsDB muser_id project_id discussion_id
        children <- fetchCommentsDescendantsDB muser_id project_id roots_ids
        return (project, page, length roots_ids + length children)

    let can_edit = fromMaybe False (userCanEditWikiPage . entityVal <$> maybe_user)

    defaultLayout $ do
        setTitle . toHtml $
            projectName project <> " : " <> wikiPageTarget page <> " | Snowdrift.coop"

        renderWiki comment_count project_handle target can_edit page

postWikiR :: Text -> Text -> Handler Html
postWikiR project_handle target = do
    now <- liftIO getCurrentTime

    (Entity user_id _, Entity project_id _, Entity page_id page) <- pageInfoRequireCanEdit project_handle target
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
                        renderWiki 0 project_handle target False $
                            WikiPage now target project_id content (Key $ PersistInt64 (-1)) Normal

                Just x | x == action -> do
                    runSYDB $ do
                        lift $
                            update $ \ p -> do
                            set p [WikiPageContent =. val content]
                            where_ $ p ^. WikiPageId ==. val page_id

                        edit_id <- createWikiEditDB user_id page_id content (Just comment)
                        -- TODO - I think there might be a race condition here...
                        either_last_edit <- lift $ insertBy $ WikiLastEdit page_id edit_id

                        if last_edit_id == wikiLastEditEdit last_edit
                         then lift (lift (alertSuccess "Updated."))
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

                            -- TODO(mitchell): new MessageType for edit conflict
                            void $ sendNotificationMessageDB MessageDirect last_editor message_text
                            void $ sendNotificationMessageDB MessageDirect user_id     message_text

                            lift (lift (alertDanger "conflicting edits (ticket created, messages sent)"))

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
    Just "closed" -> go fetchAllClosedRootCommentsDB
    _             -> go fetchAllOpenRootCommentsDB
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

    (Entity project_id project, Entity _ page) <- runYDB (pageInfo project_handle target)

    (roots, replies, user_map, closure_map, ticket_map, flag_map, tag_map) <- runDB $ do
        roots           <- get_root_comments muser_id project_id (wikiPageDiscussion page)
        replies         <- fetchCommentsDescendantsDB muser_id project_id (map entityKey roots)
        user_map        <- entitiesMap <$> fetchUsersInDB (S.toList $ makeCommentUsersSet roots <> makeCommentUsersSet replies)
        let comment_ids  = map entityKey (roots ++ replies)
        closure_map     <- makeClosureMapDB comment_ids
        ticket_map      <- makeTicketMapDB  comment_ids
        flag_map        <- makeFlagMapDB    comment_ids
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
    (project_entity, Entity _ page) <- runYDB $ pageInfo project_handle target

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
    (Entity _ project, Entity page_id _) <- runYDB $ pageInfo project_handle target

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
    (_, Entity _ project, Entity page_id page) <- pageInfoRequireCanEdit project_handle target
    Entity _ last_edit <- runYDB $ getBy404 $ UniqueWikiLastEdit page_id

    (wiki_form, _) <- generateFormPost $ editWikiForm (wikiLastEditEdit last_edit) (wikiPageContent page) Nothing

    defaultLayout $ do
        setTitle . toHtml $ projectName project <> " Wiki - " <> wikiPageTarget page <> " | Snowdrift.coop"
        $(widgetFile "edit_wiki")

--------------------------------------------------------------------------------
-- /#target/h

getWikiHistoryR :: Text -> Text -> Handler Html
getWikiHistoryR project_handle target = do
    (Entity _ project, Entity page_id _) <- runYDB $ pageInfo project_handle target

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
    (Entity _ project, Entity page_id _) <- runYDB $ pageInfo project_handle target
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
    (_, Entity _ project) <- pageInfoRequireAffiliation' project_handle
    (wiki_form, _) <- generateFormPost $ newWikiForm Nothing
    defaultLayout $ do
        setTitle . toHtml $ projectName project <> " Wiki - New Page | Snowdrift.coop"
        $(widgetFile "new_wiki")


postNewWikiR :: Text -> Text -> Handler Html
postNewWikiR project_handle target = do
    (Entity user_id _, Entity project_id _) <- pageInfoRequireAffiliation' project_handle
    now <- liftIO getCurrentTime
    ((result, _), _) <- runFormPost $ newWikiForm Nothing
    case result of
        FormSuccess content -> do
            mode <- lookupPostParam "mode"
            let action :: Text = "create"
            case mode of
                Just "preview" -> do
                        (form, _) <- generateFormPost $ newWikiForm (Just content)
                        defaultLayout $ do
                            let page = WikiPage now target project_id content (Key $ PersistInt64 0) Normal
                            previewWidget form action $ renderWiki 0 project_handle target False page

                Just x | x == action -> do
                    runSDB (createWikiPageDB target project_id content Normal user_id)

                    alertSuccess "Created."
                    redirect $ WikiR project_handle target

                _ -> error "unrecognized mode"

        FormMissing -> error "Form missing."
        FormFailure msgs -> error $ "Error submitting form: " ++ T.unpack (T.concat msgs)

--------------------------------------------------------------------------------
-- /#target/perm

getEditWikiPermissionsR :: Text -> Text -> Handler Html
getEditWikiPermissionsR project_handle target = do
    (_, Entity _ project, Entity _ page) <- pageInfoRequireAffiliation project_handle target
    (wiki_form, _) <- generateFormPost $ editWikiPermissionsForm (wikiPagePermissionLevel page)

    defaultLayout $ do
        setTitle . toHtml $ projectName project <> " Wiki Permissions - " <> target <> " | Snowdrift.coop"
        $(widgetFile "edit_wiki_perm")

postEditWikiPermissionsR :: Text -> Text -> Handler Html
postEditWikiPermissionsR project_handle target = do
    (_, _, Entity page_id page) <- pageInfoRequireAffiliation project_handle target
    ((result, _), _) <- runFormPost $ editWikiPermissionsForm (wikiPagePermissionLevel page)

    case result of
        FormSuccess level -> do
            runDB $
                update $ \ p -> do
                where_ $ p ^. WikiPageId ==. val page_id
                set p [ WikiPagePermissionLevel =. val level ]

            alertSuccess "permissions updated"

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
