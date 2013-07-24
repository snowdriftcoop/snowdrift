{-# LANGUAGE TupleSections #-}

module Handler.Wiki where

import Import

import Widgets.Sidebar
import Widgets.Markdown
import Widgets.Time

import Control.Applicative ((<|>))

import Model.Role (Role (..), roleField)
import Model.User

import Yesod.Markdown

import qualified Data.Text as T

import qualified Data.Foldable as F

import qualified Data.Set as S
import qualified Data.Map as M

import Data.Tree

import Control.Arrow ((&&&))

import Database.Persist.Store

import Data.Time

import Data.Algorithm.Diff (getDiff, Diff (..))

import Data.Function (on)

import Text.Blaze.Html5 (ins, del, br)

getWikiR :: Text -> Handler RepHtml
getWikiR target = do
    Entity _ page <- runDB $ do
        page_entity <- getBy404 $ UniqueWikiTarget target
        return page_entity

    let can_view = wikiPageCanView page

    (can_edit, can_view_meta) <- if can_view == Public
         then do
            maybe_user <- maybeAuth
            case maybe_user of
                Nothing -> return (False, False)
                Just (Entity _ user) -> return (userRole user >= wikiPageCanEdit page, userRole user >= wikiPageCanViewMeta page)
                    
        
         else do
            Entity _ user <- requireAuth
            when (userRole user < can_view) $ permissionDenied "You do not have sufficient privileges to view this page."

            return (userRole user >= wikiPageCanEdit page, userRole user >= wikiPageCanViewMeta page)

    defaultLayout $ renderWiki target can_edit can_view_meta page


getWikiPagesR :: Handler RepHtml
getWikiPagesR = do
    Entity _ user <- requireAuth

    unfiltered_pages <- runDB $ selectList [] [Asc WikiPageTarget]

    let pages = filter (\ page -> wikiPageCanView (entityVal page) <= userRole user) unfiltered_pages

    defaultLayout $ $(widgetFile "wiki_pages")


renderWiki :: Text -> Bool -> Bool -> WikiPage -> Widget
renderWiki target can_edit can_view_meta page = $(widgetFile "wiki")



postWikiR :: Text -> Handler RepHtml
postWikiR target = do
    Entity user_id user <- requireAuth
    now <- liftIO getCurrentTime

    (Entity page_id page, Entity _ last_edit) <- runDB $ do
        page_entity <- getBy404 $ UniqueWikiTarget target
        last_edit_entity <- getBy404 $ UniqueWikiLastEdit $ entityKey page_entity
        return (page_entity, last_edit_entity)

    when (userRole user < wikiPageCanEdit page) $ permissionDenied "You do not have sufficient privileges to edit this page."

    ((result, _), _) <- runFormPost $ editWikiForm (wikiLastEditEdit last_edit) (wikiPageContent page) Nothing


    case result of
        FormSuccess (last_edit_id, content, comment) ->
            if last_edit_id == wikiLastEditEdit last_edit
             then do
                mode <- lookupPostParam "mode"

                let action :: Text = "update"

                case mode of
                    Just "preview" -> do
                        (form, _) <- generateFormPost $ editWikiForm (wikiLastEditEdit last_edit) content comment
                        defaultLayout $ renderPreview form action $ renderWiki target False False $ WikiPage target content Uninvited Uninvited Uninvited

                    Just x | x == action -> do
                        runDB $ do
                            update page_id [WikiPageContent =. content]
                            edit_id <- insert $ WikiEdit now user_id page_id content comment
                            either_last_edit <- insertBy $ WikiLastEdit page_id edit_id
                            case either_last_edit of
                                Left (Entity to_update _) -> update to_update [WikiLastEditEdit =. edit_id]
                                Right _ -> return ()

                        setMessage "Updated."
                        redirect $ WikiR target

                    _ -> error "Error: unrecognized mode"

             else
                error "Error submitting form - page was updated since you last saw it" -- TODO: something better here

        FormMissing -> error "Form missing."
        FormFailure msgs -> error $ "Error submitting form: " ++ T.unpack (T.concat msgs)


getEditWikiR :: Text -> Handler RepHtml
getEditWikiR target = do
    Entity user_id user <- requireAuth
    (Entity page_id page, Entity _ last_edit) <- runDB $ do
        page_entity <- getBy404 $ UniqueWikiTarget target
        last_edit_entity <- getBy404 $ UniqueWikiLastEdit $ entityKey page_entity
        return (page_entity, last_edit_entity)

    when (userRole user < wikiPageCanEdit page) $ permissionDenied "You do not have sufficient privileges to edit this page."

    (wiki_form, _) <- generateFormPost $ editWikiForm (wikiLastEditEdit last_edit) (wikiPageContent page) Nothing

    defaultLayout $(widgetFile "edit_wiki")


getNewWikiR :: Text -> Handler RepHtml
getNewWikiR target = do
    Entity user_id user <- requireAuth

    when (userRole user < CommitteeMember) $ permissionDenied "You do not have sufficient privileges to create a new page."

    (wiki_form, _) <- generateFormPost $ newWikiForm Nothing Nothing Nothing Nothing

    defaultLayout $(widgetFile "new_wiki")


postNewWikiR :: Text -> Handler RepHtml
postNewWikiR target = do
    Entity user_id user <- requireAuth
    when (userRole user < CommitteeMember) $ permissionDenied "You do not have sufficient privileges to create a new page."
    now <- liftIO getCurrentTime

    ((result, _), _) <- runFormPost $ newWikiForm Nothing Nothing Nothing Nothing

    case result of
        FormSuccess (content, can_view, can_view_meta, can_edit) -> do
            mode <- lookupPostParam "mode"
            let action :: Text = "create"
            case mode of
                Just "preview" -> do
                        (form, _) <- generateFormPost $ newWikiForm (Just content) (Just can_view) (Just can_view_meta) (Just can_edit)
                        defaultLayout $ renderPreview form action $ renderWiki target False False $ WikiPage target content Uninvited Uninvited Uninvited


                Just x | x == action -> do
                    _ <- runDB $ do
                        page_id <- insert $ WikiPage target content can_view can_view_meta can_edit
                        edit_id <- insert $ WikiEdit now user_id page_id content $ Just "Page created."
                        insert $ WikiLastEdit page_id edit_id

                    setMessage "Created."
                    redirect $ WikiR target

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


getDiscussWikiR :: Text -> Handler RepHtml
getDiscussWikiR target = do
    Entity user_id user <- requireAuth
    Entity page_id page  <- runDB $ getBy404 $ UniqueWikiTarget target

    let can_edit = userRole user >= wikiPageCanEdit page

    (roots, rest, users, retraction_map) <- runDB $ do
        roots <- selectList [ CommentPage ==. page_id, CommentParent ==. Nothing ] [Asc CommentCreatedTs]
        rest <- selectList [ CommentPage ==. page_id, CommentParent !=. Nothing ] [Asc CommentParent, Asc CommentCreatedTs]

        let get_user_ids = S.fromList . map (commentUser . entityVal) . F.toList
            user_id_list = S.toList $ get_user_ids roots `S.union` get_user_ids rest

        user_entities <- selectList [ UserId <-. user_id_list ] []

        let users = M.fromList $ map (entityKey &&& id) user_entities

        retraction_map <- M.fromList . map ((commentRetractionComment &&& id) . entityVal) <$> selectList [ CommentRetractionComment <-. map entityKey (roots ++ rest) ] []
        return (roots, rest, users, retraction_map)

    let comments = forM_ roots $ \ root -> renderComment user_id target users 10 0 [] retraction_map $ buildCommentTree root rest

    (comment_form, _) <- generateFormPost $ commentForm Nothing Nothing

    defaultLayout $(widgetFile "wiki_discuss")


getDiscussCommentR :: Text -> CommentId -> Handler RepHtml
getDiscussCommentR target comment_id = do
    Entity user_id user <- requireAuth
    Entity page_id page  <- runDB $ getBy404 $ UniqueWikiTarget target

    let can_edit = userRole user >= wikiPageCanEdit page

    (root, rest, users, earlier_retractions, retraction_map) <- runDB $ do
        root <- get404 comment_id

        when (commentPage root /= page_id) $ error "Selected comment does not match selected page"

        subtree <- selectList [CommentAncestorAncestor ==. comment_id] []
        rest <- selectList [CommentPage ==. page_id, CommentParent !=. Nothing, CommentId >. comment_id, CommentId <-. map (commentAncestorComment . entityVal) subtree] [Asc CommentParent, Asc CommentCreatedTs]

        let get_user_ids = S.fromList . map (commentUser . entityVal) . F.toList
            user_id_list = S.toList $ S.insert (commentUser root) $ get_user_ids rest

        user_entities <- selectList [ UserId <-. user_id_list ] []

        let users = M.fromList $ map (entityKey &&& id) user_entities

        ancestors <- selectList [ CommentAncestorComment ==. comment_id ] []
        earlier_retractions <- map entityVal <$> selectList [ CommentRetractionComment <-. map (commentAncestorAncestor . entityVal) ancestors ] []
        retraction_map <- M.fromList . map ((commentRetractionComment &&& id) . entityVal) <$> selectList [ CommentRetractionComment <-. comment_id : map entityKey rest ] []
        
        return (root, rest, users, earlier_retractions, retraction_map)

    (comment_form, _) <- generateFormPost $ commentForm (Just comment_id) Nothing

    defaultLayout $ renderDiscussComment user_id target can_edit comment_form (Entity comment_id root) rest users earlier_retractions retraction_map


renderDiscussComment :: UserId -> Text -> Bool -> Widget -> Entity Comment -> [Entity Comment] -> M.Map UserId (Entity User) -> [CommentRetraction] -> M.Map CommentId CommentRetraction -> Widget
renderDiscussComment viewer_id target can_edit comment_form root rest users earlier_retractions retraction_map = do
    let Node parent children = buildCommentTree root rest
        comment = renderComment viewer_id target users 1 0 earlier_retractions retraction_map $ Node parent []
        child_comments = mapM_ (renderComment viewer_id target users 10 0 [] retraction_map) children

    $(widgetFile "comment")



postDiscussWikiR :: Text -> Handler RepHtml
postDiscussWikiR target = do
    Entity user_id user <- requireAuth
    Entity page_id _ <- runDB $ getBy404 $ UniqueWikiTarget target

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
                                ancestors <- (parent_id :) . map (commentAncestorAncestor . entityVal) <$> selectList [ CommentAncestorComment ==. parent_id ] []
                                map entityVal <$> selectList [ CommentRetractionComment <-. ancestors ] []

                            Nothing -> return []


                    (form, _) <- generateFormPost $ commentForm maybe_parent_id (Just text)
                    defaultLayout $ renderPreview form action $ renderDiscussComment user_id target False (return ()) (Entity (Key $ PersistInt64 0) $ Comment now page_id maybe_parent_id user_id text depth) [] (M.singleton user_id $ Entity user_id user) earlier_retractions M.empty


                Just x | x == action -> do
                    runDB $ do
                        comment_id <- insert $ Comment now page_id maybe_parent_id user_id text depth
                        
                        let content = T.lines $ (\ (Markdown str) -> str) text
                            tickets = map T.strip $ mapMaybe (T.stripPrefix "ticket:") content
                            tags = map T.strip $ mconcat $ map (T.splitOn ",") $ mapMaybe (T.stripPrefix "tags:") content

                        forM_ tickets $ \ ticket -> insert $ Ticket now ticket comment_id
                        forM_ tags $ \ tag -> do
                            tag_id <- fmap (either entityKey id) $ insertBy $ Tag tag
                            insert $ CommentTag comment_id tag_id user_id

                        let getParentAncestors parent_id = (parent_id :) . map (commentAncestorAncestor . entityVal) <$> selectList [ CommentAncestorComment ==. parent_id ] []

                        ancestors <- maybe (return []) getParentAncestors maybe_parent_id

                        forM_ ancestors $ \ ancestor_id -> insert $ CommentAncestor comment_id ancestor_id

                    setMessage "comment posted"
                    redirect $ DiscussWikiR target

                _ -> error "unrecognized mode"

        FormMissing -> error "Form missing."
        FormFailure msgs -> error $ "Error submitting form: " ++ T.unpack (T.intercalate "\n" msgs)


getWikiNewCommentsR :: Handler RepHtml
getWikiNewCommentsR = do
    Entity user_id user <- requireAuth

    maybe_from <- fmap (Key . PersistInt64 . read . T.unpack) <$> lookupGetParam "from"

    now <- liftIO getCurrentTime

    (comments, pages, users, retraction_map) <- runDB $ do
        unfiltered_pages :: [Entity WikiPage] <- selectList [] []

        let pages = M.fromList $ map (entityKey &&& id) $ filter ((userRole user >=) . wikiPageCanViewMeta . entityVal) unfiltered_pages
            filters = case maybe_from of
                        Nothing -> [ CommentPage <-. M.keys pages ]
                        Just from -> [ CommentPage <-. M.keys pages, CommentId <=. from ]

        comments <- selectList filters [ Desc CommentId, LimitTo 50 ]

        let user_ids = S.toList $ S.fromList $ map (commentUser . entityVal) comments
        users <- fmap (M.fromList . map (entityKey &&& id)) $ selectList [ UserId <-. user_ids ] []

        retraction_map <- M.fromList . map ((commentRetractionComment &&& id) . entityVal) <$> selectList [ CommentRetractionComment <-. map entityKey comments ] []

        return (comments, pages, users, retraction_map)

    let PersistInt64 to = unKey $ minimum (map entityKey comments)
        rendered_comments =
            if null comments
             then [whamlet|no new comments|]
             else forM_ comments $ \ (Entity comment_id comment) -> do
                earlier_retractions <- lift $ runDB $ do
                    ancestors <- selectList [ CommentAncestorComment ==. comment_id ] []
                    map entityVal <$> selectList [ CommentRetractionComment <-. map (commentAncestorAncestor . entityVal) ancestors ] [ Asc CommentRetractionComment ]

                let target = wikiPageTarget $ entityVal $ pages M.! commentPage comment
                    rendered_comment = renderComment user_id target users 1 0 earlier_retractions retraction_map $ Node (Entity comment_id comment) []


                [whamlet|$newline never
                    <div .row>
                        <div .span9>
                            On #
                            <a href="@{WikiR target}">
                                #{target}
                            :
                            ^{rendered_comment}
                |]

    runDB $ update user_id [ UserReadComments =. now ]

    defaultLayout $(widgetFile "wiki_new_comments")


getWikiHistoryR :: Text -> Handler RepHtml
getWikiHistoryR target = do
    (edits, users) <- runDB $ do
        Entity page_id _ <- getBy404 $ UniqueWikiTarget target
        edits <- selectList [ WikiEditPage ==. page_id ] [ Desc WikiEditId ]

        let user_id_list = S.toList $ S.fromList $ map (wikiEditUser . entityVal) edits

        users <- fmap (M.fromList . map (entityKey &&& id)) $ selectList [ UserId <-. user_id_list ] []

        return (edits, users)

    let editsIndexed = zip ([0..] :: [Int]) edits
    defaultLayout $(widgetFile "wiki_history")

-- | A proxy handler that redirects "ugly" to "pretty" diff URLs,
-- e.g. /w/diff?from=a&to=b to /w/diff/a/b
getWikiDiffProxyR :: Text -> Handler RepHtml
getWikiDiffProxyR target = do
    (start_edit_id_t, end_edit_id_t) <- runInputGet $ (,)
                                        <$> ireq textField "start"
                                        <*> ireq textField "end"
    let pairMay = do
        s <- fromPathPiece start_edit_id_t
        e <- fromPathPiece end_edit_id_t
        return (s, e)
    maybe
        (invalidArgs ["revision IDs"])
        (\(s, e) -> redirect $ WikiDiffR target s e)
        pairMay

getWikiDiffR :: Text -> WikiEditId -> WikiEditId -> Handler RepHtml
getWikiDiffR target start_edit_id end_edit_id = do
    (start_edit, end_edit) <- runDB $ do
        Entity page_id _ <- getBy404 $ UniqueWikiTarget target
        start_edit <- get404 start_edit_id
        end_edit <- get404 end_edit_id

        when (page_id /= wikiEditPage start_edit) $ error "selected 'start' edit is not an edit of selected page"
        when (page_id /= wikiEditPage end_edit) $ error "selected 'end' edit is not an edit of selected page"

        return (start_edit, end_edit)

    let diffEdits = getDiff `on` ((\ (Markdown text) -> T.lines text) . wikiEditContent)
        renderDiff = mconcat . map (\ a -> (case a of Both x _ -> toHtml x; First x -> del (toHtml x); Second x -> ins (toHtml x)) >> br)

    defaultLayout $(widgetFile "wiki_diff")

getWikiEditR :: Text -> WikiEditId -> Handler RepHtml
getWikiEditR target edit_id = do
    edit <- runDB $ do
        Entity page_id _ <- getBy404 $ UniqueWikiTarget target
        edit <- get404 edit_id

        when (page_id /= wikiEditPage edit) $ error "selected edit is not an edit of selected page"

        return edit

    defaultLayout $(widgetFile "wiki_edit")

getWikiNewEditsR :: Handler RepHtml
getWikiNewEditsR = do
    Entity user_id user <- requireAuth

    maybe_from <- fmap (Key . PersistInt64 . read . T.unpack) <$> lookupGetParam "from"

    now <- liftIO getCurrentTime
    (edits, pages, users) :: ([Entity WikiEdit], M.Map WikiPageId (Entity WikiPage), M.Map UserId (Entity User)) <- runDB $ do
        unfiltered_pages :: [Entity WikiPage] <- selectList [] []
        let pages = M.fromList $ map (entityKey &&& id) $ filter ((userRole user >=) . wikiPageCanViewMeta . entityVal) unfiltered_pages
            filters = case maybe_from of
                        Nothing -> [ WikiEditPage <-. M.keys pages ]
                        Just from -> [ WikiEditPage <-. M.keys pages, WikiEditId <=. from ]
        edits <- selectList filters [ Desc WikiEditId, LimitTo 50 ]

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
                            <a href="@{WikiEditR (wikiPageTarget (entityVal page)) edit_id}">
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

    runDB $ update user_id [ UserReadEdits =. now ]

    defaultLayout $(widgetFile "wiki_new_edits")


getRetractCommentR :: Text -> CommentId -> Handler RepHtml
getRetractCommentR target comment_id = do
    Entity user_id user <- requireAuth
    comment <- runDB $ get404 comment_id
    when (commentUser comment /= user_id) $ permissionDenied "You can only retract your own comments."

    earlier_retractions <- runDB $
        case commentParent comment of
            Just parent_id -> do
                ancestors <- (parent_id :) . map (commentAncestorAncestor . entityVal) <$> selectList [ CommentAncestorComment ==. parent_id ] []
                map entityVal <$> selectList [ CommentRetractionComment <-. ancestors ] []

            Nothing -> return []


    (retract_form, _) <- generateFormPost $ retractForm Nothing
    let rendered_comment = renderDiscussComment user_id target False (return ()) (Entity comment_id comment) [] (M.singleton user_id $ Entity user_id user) earlier_retractions M.empty

    defaultLayout $ [whamlet|
        ^{rendered_comment}
        <form method="POST">
            ^{retract_form}
            <input type="submit" name="mode" value="preview retraction">
    |]

postRetractCommentR :: Text -> CommentId -> Handler RepHtml
postRetractCommentR target comment_id = do
    Entity user_id user <- requireAuth
    comment <- runDB $ get404 comment_id
    when (commentUser comment /= user_id) $ permissionDenied "You can only retract your own comments."

    ((result, _), _) <- runFormPost $ retractForm Nothing

    case result of
        FormSuccess reason -> do
            earlier_retractions <- runDB $
                case commentParent comment of
                    Just parent_id -> do
                        ancestors <- (parent_id :) . map (commentAncestorAncestor . entityVal) <$> selectList [ CommentAncestorComment ==. parent_id ] []
                        map entityVal <$> selectList [ CommentRetractionComment <-. ancestors ] []

                    Nothing -> return []

            let action :: Text = "retract"
            mode <- lookupPostParam "mode"
            case mode of
                Just "preview retraction" -> do
                    (form, _) <- generateFormPost $ retractForm (Just reason)
                    let soon = UTCTime (ModifiedJulianDay 0) 0
                        retraction = CommentRetraction soon reason comment_id
                        
                    defaultLayout $ renderPreview form action $ renderDiscussComment user_id target False (return ()) (Entity comment_id comment) [] (M.singleton user_id $ Entity user_id user) earlier_retractions $ M.singleton comment_id retraction


                Just a | a == action -> do
                    now <- liftIO getCurrentTime
                    _ <- runDB $ insert $ CommentRetraction now reason comment_id

                    redirect $ DiscussCommentR target comment_id

                _ -> error "Error: unrecognized mode."
        _ -> error "Error when submitting form."

retractForm :: Maybe Markdown -> Form Markdown
retractForm reason = renderDivs $ areq snowdriftMarkdownField "Retraction reason:" reason
    
renderComment :: UserId -> Text -> M.Map UserId (Entity User) -> Int -> Int -> [CommentRetraction] -> M.Map CommentId CommentRetraction -> Tree (Entity Comment) -> Widget
renderComment viewer_id target users max_depth depth earlier_retractions retraction_map tree = do
    maybe_route <- lift getCurrentRoute

    let Entity comment_id comment = rootLabel tree
        children = subForest tree

        Entity user_id user = users M.! commentUser comment
        author_name = userPrintName (Entity user_id user)
        comment_time = renderTime (commentCreatedTs comment)

        top_level = (commentDepth comment == 0)
        even_depth = not top_level && commentDepth comment `mod` 2 == 1
        odd_depth = not top_level && not even_depth

        maybe_retraction = M.lookup comment_id retraction_map
        empty_list = []

     in $(widgetFile "comment_body")


countReplies :: [Tree a] -> Int
countReplies = sum . map (F.sum . fmap (const 1))


editWikiForm :: WikiEditId -> Markdown -> Maybe Text -> Form (WikiEditId, Markdown, Maybe Text)
editWikiForm last_edit_id content comment = renderDivs $ (,,)
        <$> areq hiddenField "" (Just last_edit_id)
        <*> areq snowdriftMarkdownField "Page Content" (Just content)
        <*> aopt textField "Comment" (Just comment)


newWikiForm :: Maybe Markdown -> Maybe Role -> Maybe Role -> Maybe Role -> Form (Markdown, Role, Role, Role)
newWikiForm content can_view can_view_meta can_edit = renderDivs $ (,,,)
        <$> areq snowdriftMarkdownField "Page Content" content
        <*> areq (roleField Admin) "Minimum Role to View" (can_view <|> Just GeneralPublic)
        <*> areq (roleField Admin) "Minimum Role to View Metadata" (can_view_meta <|> Just CommitteeCandidate)
        <*> areq (roleField Admin) "Minimum Role to Edit" (can_edit <|> Just CommitteeMember)


disabledCommentForm :: Form Markdown
disabledCommentForm = renderDivs $ areq snowdriftMarkdownField ("Reply" { fsAttrs = [("disabled","")] }) Nothing

commentForm :: Maybe CommentId -> Maybe Markdown -> Form (Maybe CommentId, Markdown)
commentForm parent content = renderDivs
    $ (,)
        <$> aopt hiddenField "" (Just parent)
        <*> areq snowdriftMarkdownField (if isNothing parent then "Comment" else "Reply") content

renderPreview :: Widget -> Text -> Widget -> Widget
renderPreview form action widget =
    [whamlet|
        <form method="POST" style="padding : 0em; margin : 0em">
            <div .row>
                <div .span9>
                    <div .alert>
                        This is a preview; your changes have not been saved!
                        You can edit it below.
                    <input type=submit name=mode value="#{action}">

            ^{widget}

            <div .row>
                <div .span9>
                    <div .alert>
                        This is a preview; your changes have not been saved!
                    <input type=submit name=mode value="preview">
                    <input type=submit name=mode value="#{action}">
                    ^{form}
                    <input type=submit name=mode value="preview">
                    <input type=submit name=mode value="#{action}">
    |]



