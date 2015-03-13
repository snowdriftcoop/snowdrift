-- | Handler for Wiki paths. Section comments are relative to /p/#handle/w

module Handler.Wiki where

import Import

import Handler.Comment
import Handler.Discussion
import Handler.Utils
import Handler.Wiki.Comment (makeWikiPageCommentForestWidget, wikiDiscussionPage)
import Model.Comment
import Model.Comment.ActionPermissions
import Model.Comment.Sql
import Model.Markdown
import Model.Notification
import Model.Permission
import Model.User
import Model.Wiki
import Widgets.Preview
import Widgets.Time
import View.Comment
import View.Wiki

import           Data.Algorithm.Diff  (getDiff, Diff (..))
import           Data.Default         (def)
import qualified Data.List            as L
import qualified Data.Map             as M
import qualified Data.Set             as S
import qualified Data.Text            as T
import           Text.Blaze.Html5     (ins, del, br)
import           Text.Cassius         (cassiusFile)

import           Yesod.Default.Config            (appRoot)

--------------------------------------------------------------------------------
-- Utility functions

-- | Get the Project entity, requiring established users.
projectInfoRequireEstablished :: Text -- ^ Project handle
                              -> Handler (Entity User, Entity Project)
projectInfoRequireEstablished project_handle = do
    user@(Entity _ user') <- requireAuth

    case userEstablished user' of
        EstEstablished _ _ _ -> return ()
        EstEligible _ _ -> permissionDenied $
            "You must accept the honor agreement before you can create a new"
            <> " wiki page"
        EstUnestablished -> permissionDenied $
            "You must be an established user to create a new wiki page"

    project <- runYDB $ getBy404 $ UniqueProjectHandle project_handle
    return (user, project)

-- | Get the Project/WikiPage entities.
pageInfo :: Text -> Language -> Text -> YDB (Entity Project, Entity WikiPage, Entity WikiTarget)
pageInfo project_handle language target = do
    project                           <- getBy404 $ UniqueProjectHandle project_handle
    Entity wiki_target_id wiki_target <- getBy404 $ UniqueWikiTarget (entityKey project) language target

    let wiki_page_id = wikiTargetPage wiki_target
    wiki_page                         <- get404 wiki_page_id

    return (project, Entity wiki_page_id wiki_page, Entity wiki_target_id wiki_target)

-- | Get the Project/WikiPage entities, but require some generic permissions,
-- failing with permissionDenied if they are not satisfied.
pageInfoRequirePermission :: Text                                                          -- Project handle.
                          -> Language
                          -> Text                                                          -- Wiki page.
                          -> (Entity User -> Entity Project -> Entity WikiPage -> DB Bool) -- Permission checker.
                          -> Handler (Entity User, Entity Project, Entity WikiPage)
pageInfoRequirePermission project_handle language target has_permission = do
    user <- requireAuth
    (project, page, ok) <- runYDB $ do
        project     <- getBy404 $ UniqueProjectHandle project_handle
        wiki_target <- getBy404 $ UniqueWikiTarget (entityKey project) language target

        let wiki_page_id = wikiTargetPage $ entityVal wiki_target
        wiki_page   <- get404 wiki_page_id

        ok <- has_permission user project (Entity wiki_page_id wiki_page)
        return (project, Entity wiki_page_id wiki_page, ok)
    unless ok (permissionDenied "You don't have permission to access this page.")
    return (user, project, page)

-- | Like pageInfoRequirePermission, but specialized to requiring Project affiliation.
pageInfoRequireAffiliation :: Text -> Language -> Text -> Handler (Entity User, Entity Project, Entity WikiPage)
pageInfoRequireAffiliation project_handle language target =
    pageInfoRequirePermission project_handle language target (\(Entity user_id _) (Entity project_id _) _ ->
      userIsAffiliatedWithProjectDB user_id project_id)


-- | Like pageInfoRequireAffiliation, but requires a user to be established.
pageInfoRequireEstablished :: Text -> Language -> Text -> Handler (Entity User, Entity Project, Entity WikiPage)
pageInfoRequireEstablished project_handle language target = do
    Entity user_id user <- requireAuth

    case userEstablished user of
        EstEstablished _ _ _ -> return ()
        EstEligible _ _ -> permissionDenied "You must accept the honor agreement before you can create a new wiki page"
        EstUnestablished -> permissionDenied "You must be an established user to create a new wiki page"

    (project, page) <- runYDB $ do
        project     <- getBy404 $ UniqueProjectHandle project_handle
        wiki_target <- getBy404 $ UniqueWikiTarget (entityKey project) language target

        let wiki_page_id = wikiTargetPage $ entityVal wiki_target
        wiki_page   <- get404 wiki_page_id
        return (project, Entity wiki_page_id wiki_page)

    return (Entity user_id user, project, page)


-- | Like pageInfoRequirePermission, but specialized to requiring that the User can edit a WikiPage.
pageInfoRequireCanEdit :: Text -> Language -> Text -> Handler (Entity User, Entity Project, Entity WikiPage)
pageInfoRequireCanEdit project_handle language target =
    pageInfoRequirePermission project_handle language target (\(Entity _ user) _ _ -> return (userCanEditWikiPage user))

--------------------------------------------------------------------------------
-- /#language/#target

getWikiR :: Text -> Language -> Text -> Handler TypedContent
getWikiR project_handle language target = do
    languages <- getLanguages

    (Entity project_id project, Entity page_id page, edits) <- runYDB $ do
        (project, Entity page_id page, _) <- pageInfo project_handle language target

        edits <- select $ from $ \ (we `InnerJoin` le) -> do
            on_ $ we ^. WikiEditId ==. le ^. WikiLastEditEdit
            where_ $ we ^. WikiEditPage ==. val page_id
            return we

        return (project, Entity page_id page, edits)

    selectRep $ do
        let Entity _ edit:_ = pickEditsByLanguage languages edits

        provideRep $ return $ wikiEditContent edit
        provideRep $ do
            maybe_user <- maybeAuth

            let can_edit = fromMaybe False (userCanEditWikiPage . entityVal <$> maybe_user)

            (comment_count, translations) <- runYDB $ do
                let muser_id      = entityKey <$> maybe_user
                    discussion_id = wikiPageDiscussion page

                case muser_id of
                    Nothing -> return ()
                    Just user_id -> do
                        is_watching <- userIsWatchingProjectDB user_id project_id
                        when is_watching $
                            userViewWikiEditsDB user_id page_id

                comment_count <- fetchCommentCountDB muser_id project_id discussion_id

                let translations = L.delete (wikiEditLanguage edit) $ map (wikiEditLanguage . entityVal) edits

                return (comment_count, translations)

            defaultLayout $ do
                setTitle . toHtml $
                    projectName project <> " : " <> target <> " | Snowdrift.coop"

                renderWiki comment_count project_handle language target can_edit translations edit

postWikiR :: Text -> Language -> Text -> Handler Html
postWikiR project_handle target_language target = do
    now <- liftIO getCurrentTime

    (Entity user_id _, _, Entity page_id page) <- pageInfoRequireCanEdit project_handle target_language target
    ((result, _), _) <- runFormPost $ editWikiForm undefined undefined Nothing


    case result of
        FormSuccess (prev_edit_id, content, comment) -> do
            mode <- lookupPostMode

            WikiEdit { wikiEditLanguage = edit_language } <- runYDB $ get404 prev_edit_id

            case mode of
                Just PostMode -> do
                    runSYDB $ do

                        [(Entity _ last_edit)] <- lift $ select $ from $ \ (we `InnerJoin` le) -> do
                            on_ $ we ^. WikiEditId ==. le ^. WikiLastEditEdit
                            where_ $ le ^. WikiLastEditPage ==. val page_id
                                &&. le ^. WikiLastEditLanguage ==. val edit_language

                            return le

                        new_edit_id <- createWikiEditDB user_id page_id edit_language content (Just comment)

                        -- TODO - I think there might be a race condition here...
                        either_last_edit <- lift $ insertBy $ WikiLastEdit page_id new_edit_id edit_language

                        if prev_edit_id == wikiLastEditEdit last_edit
                         then lift $ lift $ alertSuccess "Updated."
                         else do
                            [ Value last_editor ] <- lift $ select $ from $ \ we -> do
                                where_ $ we ^. WikiEditId ==. val (wikiLastEditEdit last_edit)
                                return $ we ^. WikiEditUser

                            render <- getUrlRender
                            let wiki edit_id = render $ WikiEditR project_handle target_language target edit_id
                                comment_body = Markdown $ T.unlines
                                    [ "ticket: edit conflict"
                                    , ""
                                    , "[original version](" <> wiki prev_edit_id <> ")"
                                    , ""
                                    , "[my version](" <> wiki new_edit_id <> ")"
                                    , ""
                                    , "[their version](" <> wiki (wikiLastEditEdit last_edit) <> ")"
                                    , ""
                                    , "(this ticket was automatically generated)"
                                    ]


                            -- TODO LangEn here should be edit_language, once we actually translate the above
                            comment_id <- lift $ insert =<< makeApprovedComment user_id (wikiPageDiscussion page) Nothing comment_body 0 VisPublic LangEn

                            lift $ insert_ $ Ticket now now "edit conflict" comment_id

                            let notif_text = Markdown $ T.intercalate "\n\n"
                                    [ T.unwords
                                        [ "Edit conflict for"
                                        , renderLanguage LangEn target_language
                                        , "version of wiki page"
                                        , "*" <> target <> "*"
                                        , "on project"
                                        , project_handle
                                        ] <> "."

                                    , "[**Ticket created**](" <> render (WikiCommentR project_handle target_language target comment_id) <> ")"
                                    ]

                            sendPreferredNotificationDB last_editor NotifEditConflict
                                Nothing Nothing notif_text
                            sendPreferredNotificationDB user_id NotifEditConflict
                                Nothing Nothing notif_text

                            lift $ lift $ alertDanger "conflicting edits (ticket created, notification sent)"

                        case either_last_edit of
                            Left (Entity to_update _) -> lift $ update $ \ l -> do
                                set l [WikiLastEditEdit =. val new_edit_id]
                                where_ $ l ^. WikiLastEditId ==. val to_update

                            Right _ -> return ()

                    redirect $ WikiR project_handle target_language target

                _ -> do

                    translations <- fmap unwrapValues $ runYDB $ select $ from $ \ (we `InnerJoin` le) -> do
                        on_ $ we ^. WikiEditId ==. le ^. WikiLastEditEdit
                        where_ $ we ^. WikiEditPage ==. val page_id
                            &&. we ^. WikiEditLanguage !=. val edit_language
                        return $ we ^. WikiEditLanguage

                    (form, _) <- generateFormPost $ editWikiForm prev_edit_id content (Just comment)
                    defaultLayout $ previewWidgetWithAgreement
                        ("By clicking the \"update\" button, you agree to irrevocably "
                      <> "release your contribution under the CC-BY-SA 4.0 License. "
                      <> "You agree that a hyperlink or URL is sufficient attribution under the Creative Commons license.")
                        form "update" $
                        renderWiki 0 project_handle target_language target False translations $
                            WikiEdit now user_id page_id edit_language content (Just comment)

        FormMissing -> error "Form missing."
        FormFailure msgs -> error $ "Error submitting form: " ++ T.unpack (T.concat msgs)


--------------------------------------------------------------------------------
-- /#language/#target/d

-- | getWikiDiscussionR generates the associated discussion page for each wiki page
getWikiDiscussionR :: Text -> Language -> Text -> Handler Html
getWikiDiscussionR project_handle language target = getDiscussion (getWikiDiscussionR' project_handle language target)

getWikiDiscussionR'
        :: Text                                                      -- ^ Project handle.
        -> Language
        -> Text                                                      -- ^ Wiki page name.
        -> (DiscussionId -> ExprCommentCond -> DB [Entity Comment])  -- ^ Root comment getter.
        -> Handler Html
getWikiDiscussionR' project_handle language target get_root_comments = do
    muser <- maybeAuth
    let muser_id = entityKey <$> muser

    (Entity project_id project, root_comments) <- runYDB $ do
        (project@(Entity project_id _), Entity _ page, _) <- pageInfo project_handle language target
        let has_permission = (exprCommentProjectPermissionFilter muser_id (val project_id))
        root_comments <- get_root_comments (wikiPageDiscussion page) has_permission
        return (project, root_comments)

    (comment_forest_no_css, _) <-
        makeWikiPageCommentForestWidget
            muser
            project_id
            project_handle
            language
            target
            root_comments
            def
            getMaxDepth
            False
            mempty

    let has_comments = not (null root_comments)
        comment_forest = do
            comment_forest_no_css
            toWidget $(cassiusFile "templates/comment.cassius")

    (comment_form, _) <- generateFormPost commentNewTopicForm

    defaultLayout $ do
        setTitle . toHtml $ projectName project <> " Wiki Discussion - " <> target <> " | Snowdrift.coop"
        $(widgetFile "wiki_discuss")

--------------------------------------------------------------------------------
-- /#language/#target/d/new

getNewWikiDiscussionR :: Text -> Language -> Text -> Handler Html
getNewWikiDiscussionR project_handle language target = do
    void requireAuth
    let widget = commentNewTopicFormWidget
    defaultLayout $(widgetFile "wiki_discussion_wrapper")

postNewWikiDiscussionR :: Text -> Language -> Text -> Handler Html
postNewWikiDiscussionR project_handle language target = do
    user <- requireAuth
    (_, Entity _ WikiPage{..}, _) <- runYDB (pageInfo project_handle language target)

    postNewComment
      Nothing
      user
      wikiPageDiscussion
      (makeProjectCommentActionPermissionsMap (Just user) project_handle def) >>= \case
        Left comment_id -> redirect (WikiCommentR project_handle language target comment_id)
        Right (widget, form) -> defaultLayout $ previewWidget form "post" (wikiDiscussionPage project_handle language target widget)

--------------------------------------------------------------------------------
-- /#language/#target/diff/#from/#to
-- /#language/#target/diffp

getWikiDiffR :: Text -> Language -> Text -> WikiEditId -> WikiEditId -> Handler Html
getWikiDiffR project_handle language target start_edit_id end_edit_id = do
    (Entity _ project, Entity page_id _, _) <- runYDB $ pageInfo project_handle language target

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
getWikiDiffProxyR :: Text -> Language -> Text -> Handler Html
getWikiDiffProxyR project_handle language target = do
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
        (\(s, e) -> redirect $ WikiDiffR project_handle language target s e)
        pairMay

--------------------------------------------------------------------------------
-- /#language/#target/edit

getEditWikiR :: Text -> Language -> Text -> Handler Html
getEditWikiR project_handle language target = do
    (_, Entity _ project, Entity page_id _) <- pageInfoRequireCanEdit project_handle language target
    (last_edit, edit) <- runYDB $ do
        -- TODO - on missing version, redirect to translate
        Entity _ last_edit <- getBy404 $ UniqueWikiLastEdit page_id language
        edit <- get404 $ wikiLastEditEdit last_edit
        return (last_edit, edit)

    (wiki_form, _) <- generateFormPost $ editWikiForm (wikiLastEditEdit last_edit) (wikiEditContent edit) Nothing

    defaultLayout $ do
        setTitle . toHtml $ projectName project <> " Wiki - " <> target <> " | Snowdrift.coop"
        $(widgetFile "edit_wiki")

--------------------------------------------------------------------------------
-- /#language/#target/h

getWikiHistoryR :: Text -> Language -> Text -> Handler Html
getWikiHistoryR project_handle language target = do
    (Entity _ project, Entity page_id _, _) <- runYDB $ pageInfo project_handle language target

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
-- /#language/#target/h/#edit

getWikiEditR :: Text -> Language -> Text -> WikiEditId -> Handler Html
getWikiEditR project_handle language target wiki_edit_id = do
    (Entity _ project, Entity wiki_page_id _, wiki_target) <- runYDB $ pageInfo project_handle language target
    wiki_edit <- runYDB $ do
        wiki_edit <- get404 wiki_edit_id

        when (wiki_page_id /= wikiEditPage wiki_edit) $ error "selected edit is not an edit of selected page"

        return wiki_edit

    let discussion = DiscussionOnWikiPage wiki_target

    defaultLayout $ do
    -- TODO: prettier date format? or edit id?
        setTitle . toHtml $ projectName project <> " Wiki - " <> target <> " at " <> T.pack (show $ wikiEditTs wiki_edit) <> " | Snowdrift.coop"
        $(widgetFile "wiki_edit")

--------------------------------------------------------------------------------
-- /#language/#target/new

getNewWikiR :: Text -> Language -> Text -> Handler Html
getNewWikiR project_handle language target = do
    (_, Entity _ project) <-
        projectInfoRequireEstablished project_handle
    (wiki_form, _) <- generateFormPost $ newWikiForm Nothing
    defaultLayout $ do
        setTitle . toHtml $
            projectName project <> " Wiki - New Page | Snowdrift.coop"
        $(widgetFile "new_wiki")


postNewWikiR :: Text -> Language -> Text -> Handler Html
postNewWikiR project_handle language target = do
    (Entity user_id _, Entity project_id _) <-
        projectInfoRequireEstablished project_handle

    now <- liftIO getCurrentTime
    ((result, _), _) <- runFormPost $ newWikiForm Nothing
    case result of
        FormSuccess content -> do
            lookupPostMode >>= \case
                Just PostMode -> do
                    runSDB
                        (createWikiPageDB language
                                          target
                                          project_id
                                          content
                                          Normal
                                          user_id)

                    alertSuccess "Created."
                    redirect $ WikiR project_handle language target

                _ -> do
                    (form, _) <- generateFormPost $ newWikiForm (Just content)

                    defaultLayout $ do
                        let wiki_page_id = key $ PersistInt64 (-1)
                            edit = WikiEdit now
                                            user_id
                                            wiki_page_id
                                            language
                                            content
                                            (Just "page created")

                        previewWidget form "create" $
                            renderWiki 0
                                       project_handle
                                       language
                                       target
                                       False
                                       []
                                       edit

        FormMissing -> error "Form missing."
        FormFailure msgs -> error $
            "Error submitting form: " ++ T.unpack (T.concat msgs)

--------------------------------------------------------------------------------
-- /#language/#target/translate

getNewWikiTranslationR :: Text -> Language -> Text -> Handler Html
getNewWikiTranslationR project_handle language target = do
    (_, Entity _ project, Entity page_id _) <- pageInfoRequireEstablished project_handle language target

    languages <- getLanguages

    edits <- runYDB $ select $ from $ \ we -> do
        where_ $ we ^. WikiEditPage ==. val page_id
        return we

    let (Entity edit_id edit:_) = pickEditsByLanguage languages edits


    (translation_form, enctype) <- generateFormPost $ newWikiTranslationForm (Just edit_id) Nothing Nothing (Just $ wikiEditContent edit) Nothing
    defaultLayout $ do
        setTitle . toHtml $ projectName project <> " Wiki - New Translation | Snowdrift.coop"
        $(widgetFile "new_wiki_translation")


postNewWikiTranslationR :: Text -> Language -> Text -> Handler Html
postNewWikiTranslationR project_handle language target = do
    (Entity user_id _, Entity project_id _, Entity page_id _) <- pageInfoRequireEstablished project_handle language target

    now <- liftIO getCurrentTime
    ((result, _), _) <- runFormPost $ newWikiTranslationForm Nothing Nothing Nothing Nothing Nothing
    case result of
        FormSuccess (edit_id, new_language, new_target, new_content, complete) -> do
            lookupPostMode >>= \case
                Just PostMode -> do
                    runSDB $ createWikiTranslationDB page_id new_language new_target project_id new_content user_id [(edit_id, complete)]

                    alertSuccess "Created."
                    redirect (WikiR project_handle new_language new_target, [("_LANG", toPathPiece new_language)])

                _ -> do
                    (form, _) <- generateFormPost $ newWikiTranslationForm (Just edit_id) (Just new_language) (Just new_target) (Just new_content) (Just complete)

                    translations <- fmap unwrapValues $ runYDB $ select $ from $ \ (we `InnerJoin` le) -> do
                        on_ $ we ^. WikiEditId ==. le ^. WikiLastEditEdit
                        where_ $ we ^. WikiEditPage ==. val page_id
                            &&. we ^. WikiEditLanguage !=. val new_language
                        return $ we ^. WikiEditLanguage

                    defaultLayout $ do
                        let wiki_page_id = key $ PersistInt64 (-1)
                            edit = WikiEdit now user_id wiki_page_id language new_content (Just "page created")

                        previewWidget form "create" $ renderWiki 0 project_handle language target False translations edit

        FormMissing -> error "Form missing."
        FormFailure msgs -> error $ "Error submitting form: " ++ T.unpack (T.concat msgs)

--------------------------------------------------------------------------------
-- /#language/#target/perm

getEditWikiPermissionsR :: Text -> Language -> Text -> Handler Html
getEditWikiPermissionsR project_handle language target = do
    (_, Entity _ project, Entity _ page) <- pageInfoRequireAffiliation project_handle language target
    (wiki_form, _) <- generateFormPost $ editWikiPermissionsForm (wikiPagePermissionLevel page)

    defaultLayout $ do
        setTitle . toHtml $ projectName project <> " Wiki Permissions - " <> target <> " | Snowdrift.coop"
        $(widgetFile "edit_wiki_perm")

postEditWikiPermissionsR :: Text -> Language -> Text -> Handler Html
postEditWikiPermissionsR project_handle language target = do
    (_, _, Entity page_id page) <- pageInfoRequireAffiliation project_handle language target
    ((result, _), _) <- runFormPost $ editWikiPermissionsForm (wikiPagePermissionLevel page)

    case result of
        FormSuccess level -> do
            runDB $
                update $ \ p -> do
                where_ $ p ^. WikiPageId ==. val page_id
                set p [ WikiPagePermissionLevel =. val level ]

            alertSuccess "permissions updated"

            redirect $ WikiR project_handle language target

        FormMissing -> error "Form missing."
        FormFailure msgs -> error $ "Error submitting form: " ++ T.unpack (T.concat msgs)

--------------------------------------------------------------------------------
-- DEPRECATED

getMonolingualWikiR :: Text -> Text -> [Text] -> Handler Html
getMonolingualWikiR = redirectPolylingualWiki $ \case
    Nothing -> notFound
    Just url@(MonolingualWikiR _ _ _) -> error $ "not found: " ++ show url

    Just url@(WikiR _ _ _)                      -> redirectSameParams url
    Just url@(WikiCommentR _ _ _ _)             -> redirectSameParams url
    Just url@(NewWikiTranslationR _ _ _)        -> redirectSameParams url
    Just url@(ClaimWikiCommentR _ _ _ _)        -> redirectSameParams url
    Just url@(CloseWikiCommentR _ _ _ _)        -> redirectSameParams url
    Just url@(DeleteWikiCommentR _ _ _ _)       -> redirectSameParams url
    Just url@(EditWikiCommentR _ _ _ _)         -> redirectSameParams url
    Just url@(FlagWikiCommentR _ _ _ _)         -> redirectSameParams url
    Just url@(ApproveWikiCommentR _ _ _ _)      -> redirectSameParams url
    Just url@(ReplyWikiCommentR _ _ _ _)        -> redirectSameParams url
    Just url@(RethreadWikiCommentR _ _ _ _)     -> redirectSameParams url
    Just url@(RetractWikiCommentR _ _ _ _)      -> redirectSameParams url
    Just url@(WikiCommentAddTagR _ _ _ _)       -> redirectSameParams url
    Just url@(WikiCommentTagsR _ _ _ _)         -> redirectSameParams url
    Just url@(WikiCommentTagR _ _ _ _ _)        -> redirectSameParams url
    Just url@(WikiCommentApplyTagR _ _ _ _)     -> redirectSameParams url
    Just url@(WikiCommentCreateTagR _ _ _ _)    -> redirectSameParams url
    Just url@(UnclaimWikiCommentR _ _ _ _)      -> redirectSameParams url
    Just url@(WikiDiscussionR _ _ _)            -> redirectSameParams url
    Just url@(NewWikiDiscussionR _ _ _)         -> redirectSameParams url
    Just url@(WikiDiffR _ _ _ _ _)              -> redirectSameParams url
    Just url@(WikiDiffProxyR _ _ _)             -> redirectSameParams url
    Just url@(EditWikiR _ _ _)                  -> redirectSameParams url
    Just url@(WikiHistoryR _ _ _)               -> redirectSameParams url
    Just url@(WikiEditR _ _ _ _)                -> redirectSameParams url
    Just url@(NewWikiR _ _ _)                   -> redirectSameParams url
    Just url@(EditWikiPermissionsR _ _ _)       -> redirectSameParams url
    Just url@(WatchWikiCommentR _ _ _ _)        -> redirectSameParams url
    Just url@(UnwatchWikiCommentR _ _ _ _)      -> redirectSameParams url
    Just url@(OldDiscussCommentR _ _ _ _)       -> redirectSameParams url
    Just url@(OldWikiEditR _ _ _ _)             -> redirectSameParams url

    -- These routes are higher in the tree - can't possibly have been generated by inserting the language
    _                                           -> error "the impossible happened"

  where
    redirectSameParams url = do
        params <- reqGetParams <$> getRequest
        redirectParams url params

redirectPolylingualWiki :: (Maybe (Route App) -> Handler Html) -> Text -> Text -> [Text] -> Handler Html
redirectPolylingualWiki fn project_handle target rest = do
    render <- getUrlRender
    app <- getYesod

    let url = render $ MonolingualWikiR project_handle "en" (target:rest)
        splitPath  = drop 1 . T.splitOn "/"
        stripQuery = fst . T.break (== '?')
        stripRoot  = fromMaybe url . T.stripPrefix (appRoot $ settings app)

    fn $ parseRoute $ (, []) $ splitPath $ stripQuery $ stripRoot url


-- This handles any links we might have to the old /history/# style links
-- just in case any exist. We could remove it if we're willing to let
-- something break or can check that there's no such links
-- (it's unlikely there's any at all, certainly if so they are
-- almost certainly internal anyway)
getOldWikiEditR :: Text -> Language -> Text -> WikiEditId -> Handler Html
getOldWikiEditR project_handle language target edit_id = redirect $ WikiEditR project_handle language target edit_id
