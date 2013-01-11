{-# LANGUAGE TupleSections #-}

module Handler.Wiki where

import Import

import Widgets.Sidebar
import Widgets.Time

import Model.Role (Role (..), roleField)
import Model.User

-- import Yesod.Markdown

import qualified Data.Text as T

import qualified Data.Foldable as F

import qualified Data.Set as S
import qualified Data.Map as M

import Data.Tree

import Control.Monad

import Control.Arrow ((&&&))

import Database.Persist.Store



getWikiR :: Text -> Handler RepHtml
getWikiR target = do
    Entity user_id user <- requireAuth

    (Entity page_id page, Entity _ last_edit) <- runDB $ do
        page_entity <- getBy404 $ UniqueWikiTarget target
        last_edit_entity <- getBy404 $ UniqueWikiLastEdit $ entityKey page_entity
        return (page_entity, last_edit_entity)


    when (userRole user < wikiPageCanView page) $ permissionDenied "You do not have sufficient privileges to view this page."

    let can_edit = userRole user >= wikiPageCanEdit page
        can_view_meta = userRole user >= wikiPageCanViewMeta page

    defaultLayout $(widgetFile "wiki")


postWikiR :: Text -> Handler RepHtml
postWikiR target = do
    Entity user_id user <- requireAuth
    now <- liftIO getCurrentTime
    runDB $ do
        Entity page_id page <- getBy404 $ UniqueWikiTarget target
        lift $ when (userRole user < wikiPageCanEdit page) $ permissionDenied "You do not have sufficient privileges to edit this page."
        last_edit_entity <- getBy404 $ UniqueWikiLastEdit page_id

        ((result, _), _) <- lift $ runFormPost $ editWikiForm (wikiLastEditEdit $ entityVal last_edit_entity) (wikiPageContent page)

        case result of
            FormSuccess (last_edit_id, content, comment) -> do
                if last_edit_id == wikiLastEditEdit (entityVal last_edit_entity)
                 then do
                    update page_id [WikiPageContent =. content]
                    edit_id <- insert $ WikiEdit now user_id page_id content comment
                    either_last_edit <- insertBy $ WikiLastEdit page_id edit_id
                    case either_last_edit of
                        Left (Entity to_update _) -> update to_update [WikiLastEditEdit =. edit_id]
                        Right _ -> return ()

                    lift $ setMessage "Updated."

                 else
                    error "Error submitting form - page was updated since you last saw it" -- TODO: something better here

            FormMissing -> error "Form missing."
            FormFailure msgs -> error $ "Error submitting form: " ++ T.unpack (T.concat msgs)

    redirect $ WikiR target


getEditWikiR :: Text -> Handler RepHtml
getEditWikiR target = do
    Entity user_id user <- requireAuth
    (Entity page_id page, Entity _ last_edit) <- runDB $ do
        page_entity <- getBy404 $ UniqueWikiTarget target
        last_edit_entity <- getBy404 $ UniqueWikiLastEdit $ entityKey page_entity
        return (page_entity, last_edit_entity)

    when (userRole user < wikiPageCanEdit page) $ permissionDenied "You do not have sufficient privileges to edit this page."

    (wiki_form, _) <- generateFormPost $ editWikiForm (wikiLastEditEdit last_edit) (wikiPageContent page)

    defaultLayout $(widgetFile "edit_wiki")


getNewWikiR :: Text -> Handler RepHtml
getNewWikiR target = do
    Entity user_id user <- requireAuth

    when (userRole user < CommitteeMember) $ permissionDenied "You do not have sufficient privileges to create a new page."

    (wiki_form, _) <- generateFormPost newWikiForm

    defaultLayout $(widgetFile "new_wiki")


postNewWikiR :: Text -> Handler RepHtml
postNewWikiR target = do
    Entity user_id user <- requireAuth
    when (userRole user < CommitteeMember) $ permissionDenied "You do not have sufficient privileges to create a new page."
    now <- liftIO getCurrentTime

    ((result, _), _) <- runFormPost newWikiForm

    case result of
        FormSuccess (content, can_view, can_view_meta, can_edit) -> do
            _ <- runDB $ do
                page_id <- insert $ WikiPage target content can_view can_view_meta can_edit
                edit_id <- insert $ WikiEdit now user_id page_id content $ Just "Page created."
                insert $ WikiLastEdit page_id edit_id

            setMessage "Created."

        FormMissing -> error "Form missing."
        FormFailure msgs -> error $ "Error submitting form: " ++ T.unpack (T.concat msgs)

    redirect $ WikiR target

buildCommentTree :: Entity WikiComment -> [ Entity WikiComment ] -> Tree (Entity WikiComment)
buildCommentTree root rest =
    let treeOfList (node, items) =
            let has_parent p = (== Just (entityKey p)) . wikiCommentParent . entityVal
                list = dropWhile (not . has_parent node) items
                (children, rest') = break (not . has_parent node) list
                items' = map (, rest') children
             in (node, items')

     in unfoldTree treeOfList (root, rest)


getDiscussWikiR :: Text -> Handler RepHtml
getDiscussWikiR target = do
    Entity user_id user <- requireAuth
    Entity page_id page  <- runDB $ getBy404 $ UniqueWikiTarget target

    let can_edit = userRole user >= wikiPageCanEdit page

    (roots, rest, users) <- runDB $ do
        roots <- selectList [ WikiCommentPage ==. page_id, WikiCommentParent ==. Nothing ] [Asc WikiCommentCreatedTs]
        rest <- selectList [ WikiCommentPage ==. page_id, WikiCommentParent !=. Nothing ] [Asc WikiCommentParent, Asc WikiCommentCreatedTs]

        let get_user_ids = S.fromList . map (wikiCommentUser . entityVal) . F.toList
            user_id_list = S.toList $ get_user_ids roots `S.union` get_user_ids rest

        user_entities <- selectList [ UserId <-. user_id_list ] []

        let users = M.fromList $ map (entityKey &&& id) user_entities

        return $ (roots, rest, users)

    let comments = forM_ roots $ \ root -> do
        renderComment target users 10 0 $ buildCommentTree root rest

    (comment_form, _) <- generateFormPost $ commentForm Nothing

    defaultLayout $(widgetFile "wiki_discuss")


getDiscussWikiCommentR :: Text -> WikiCommentId -> Handler RepHtml
getDiscussWikiCommentR target comment_id = do
    Entity user_id user <- requireAuth
    Entity page_id page  <- runDB $ getBy404 $ UniqueWikiTarget target

    let can_edit = userRole user >= wikiPageCanEdit page

    (root, rest, users) <- runDB $ do
        root <- get404 comment_id

        when (wikiCommentPage root /= page_id) $ error "Selected comment does not match selected page"

        rest <- selectList [WikiCommentPage ==. page_id, WikiCommentParent !=. Nothing, WikiCommentId >. comment_id] [Asc WikiCommentParent, Asc WikiCommentCreatedTs]

        let get_user_ids = S.fromList . map (wikiCommentUser . entityVal) . F.toList
            user_id_list = S.toList $ S.insert (wikiCommentUser root) $ get_user_ids rest

        user_entities <- selectList [ UserId <-. user_id_list ] []

        let users = M.fromList $ map (entityKey &&& id) user_entities

        return $ (Entity comment_id root, rest, users)

    let Node parent children = buildCommentTree root rest
        comment = renderComment target users 1 0 $ Node parent []
        child_comments = mapM_ (renderComment target users 10 0) children

    (comment_form, _) <- generateFormPost $ commentForm $ Just comment_id

    defaultLayout $(widgetFile "wiki_comment")


postDiscussWikiR :: Text -> Handler RepHtml
postDiscussWikiR target = do
    Entity user_id _ <- requireAuth
    Entity page_id _ <- runDB $ getBy404 $ UniqueWikiTarget target

    now <- liftIO getCurrentTime

    ((result, _), _) <- runFormPost $ commentForm Nothing

    case result of
        FormSuccess (parent, text) -> do
            _ <- runDB $ insert $ WikiComment now Nothing page_id parent user_id text Nothing
            setMessage "comment posted"

        FormMissing -> error "Form missing."
        FormFailure msgs -> error $ "Error submitting form: " ++ T.unpack (T.concat msgs)

    redirect $ DiscussWikiR target

getWikiNewCommentsR :: Handler RepHtml
getWikiNewCommentsR = do
    Entity _ user <- requireAuth

    maybe_from <- fmap (Key . PersistInt64 . read . T.unpack) <$> lookupGetParam "from"

    (comments, pages, users) :: ([Entity WikiComment], M.Map WikiPageId (Entity WikiPage), M.Map UserId (Entity User)) <- runDB $ do
        unfiltered_pages :: [Entity WikiPage] <- selectList [] []
        let pages = M.fromList $ map (entityKey &&& id) $ filter ((userRole user >=) . wikiPageCanViewMeta . entityVal) unfiltered_pages
            filters = case maybe_from of
                        Nothing -> [ WikiCommentPage <-. M.keys pages ]
                        Just from -> [ WikiCommentPage <-. M.keys pages, WikiCommentId <=. from ]
        comments <- selectList filters [ Desc WikiCommentId, LimitTo 50 ]

        let user_ids = S.toList $ S.fromList $ map (wikiCommentUser . entityVal) comments
        users <- fmap M.fromList $ fmap (map (entityKey &&& id)) $ selectList [ UserId <-. user_ids ] []
        return (comments, pages, users)

    let PersistInt64 to = unKey $ minimum (map entityKey comments)
        rendered_comments =
            if null comments
             then [whamlet|no new comments|]
             else mapM_ (\ comment -> renderComment (wikiPageTarget $ entityVal $ pages M.! wikiCommentPage (entityVal comment)) users 1 0 $ Node comment []) comments

    defaultLayout $(widgetFile "wiki_new_comments")
    
        
getWikiHistoryR :: Text -> Handler RepHtml
getWikiHistoryR target = do
    (edits, users) <- runDB $ do
        Entity page_id _ <- getBy404 $ UniqueWikiTarget target
        edits <- selectList [ WikiEditPage ==. page_id ] [ Desc WikiEditId ]

        let user_id_list = S.toList $ S.fromList $ map (wikiEditUser . entityVal) $ edits

        users <- fmap M.fromList $ fmap (map (entityKey &&& id)) $ selectList [ UserId <-. user_id_list ] []

        return (edits, users)

    defaultLayout $(widgetFile "wiki_history")

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
    Entity _ user <- requireAuth

    maybe_from <- fmap (Key . PersistInt64 . read . T.unpack) <$> lookupGetParam "from"

    (edits, pages, users) :: ([Entity WikiEdit], M.Map WikiPageId (Entity WikiPage), M.Map UserId (Entity User)) <- runDB $ do
        unfiltered_pages :: [Entity WikiPage] <- selectList [] []
        let pages = M.fromList $ map (entityKey &&& id) $ filter ((userRole user >=) . wikiPageCanViewMeta . entityVal) unfiltered_pages
            filters = case maybe_from of
                        Nothing -> [ WikiEditPage <-. M.keys pages ]
                        Just from -> [ WikiEditPage <-. M.keys pages, WikiEditId <=. from ]
        edits <- selectList filters [ Desc WikiEditId, LimitTo 50 ]

        let user_ids = S.toList $ S.fromList $ map (wikiEditUser . entityVal) edits
        users <- fmap M.fromList $ fmap (map (entityKey &&& id)) $ selectList [ UserId <-. user_ids ] []
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

                |]

    defaultLayout $(widgetFile "wiki_new_edits")
    

renderComment :: Text -> M.Map UserId (Entity User) -> Int -> Int -> Tree (Entity WikiComment) -> Widget
renderComment target users max_depth depth tree = do
    maybe_route <- lift getCurrentRoute

    let Entity comment_id comment = rootLabel tree
        children = subForest tree

        Entity user_id user = users M.! wikiCommentUser comment
        author_name = userPrintName (Entity user_id user)
        comment_time = renderTime (wikiCommentCreatedTs comment)

     in $(widgetFile "wiki_comment_body")


countReplies :: [Tree a] -> Int
countReplies = sum . map (F.sum . fmap (const 1))


editWikiForm :: WikiEditId -> Markdown -> Form (WikiEditId, Markdown, Maybe Text)
editWikiForm last_edit_id content = renderDivs
    $ (,,)
        <$> areq hiddenField "" (Just last_edit_id)
        <*> areq markdownField "Page Content" (Just content)
        <*> aopt textField "Comment" Nothing

newWikiForm :: Form (Markdown, Role, Role, Role)
newWikiForm = renderDivs $ (,,,)
        <$> areq markdownField "Page Content" Nothing
        <*> areq (roleField Admin) "Minimum Role to View" (Just GeneralPublic)
        <*> areq (roleField Admin) "Minimum Role to View Metadata" (Just CommitteeCandidate)
        <*> areq (roleField Admin) "Minimum Role to Edit" (Just CommitteeMember)

commentForm :: Maybe WikiCommentId -> Form (Maybe WikiCommentId, Markdown)
commentForm parent = renderDivs
    $ (,)
        <$> aopt hiddenField "" (Just parent)
        <*> areq markdownField (if parent == Nothing then "Comment" else "Reply") Nothing
