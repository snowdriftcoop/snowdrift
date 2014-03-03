{-# LANGUAGE TupleSections #-}

module Handler.Discussion where

import Import

import Data.Time

import Data.Tree

import qualified Data.Foldable as F
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Text as T

import Model.AnnotatedTag
import Model.User
import Model.Role
import Model.ViewType

import Widgets.Markdown
import Widgets.Preview
import Widgets.Tag
import Widgets.Time

import Handler.Wiki

import Yesod.Markdown
import Model.Markdown

import Yesod.Default.Config

import Control.Monad.Trans.Resource

getTags :: CommentId -> Handler [Entity Tag]
getTags comment_id = runDB $ select $ from $ \ (comment_tag `InnerJoin` tag) -> do
    on_ $ comment_tag ^. CommentTagTag ==. tag ^. TagId
    where_ $ comment_tag ^. CommentTagComment ==. val comment_id
    return tag

getCommentPageId :: (MonadLogger m, MonadIO m, MonadBaseControl IO m, MonadUnsafeIO m, MonadThrow m) => CommentId -> SqlPersistT m WikiPageId
getCommentPageId comment_id = do
    [ Value page_id ] <- select $ from $ \ (c `InnerJoin` p) -> do
        on_ $ c ^. CommentDiscussion ==. p ^. WikiPageDiscussion
        where_ $ c ^. CommentId ==. val comment_id
        return $ p ^. WikiPageId

    return page_id


checkCommentPage :: CommentId -> WikiPageId -> Handler ()
checkCommentPage comment_id page_id = do
    comment_page_id <- runDB $ getCommentPageId comment_id
    when (comment_page_id /= page_id) $ error "comment does not match page"


requireModerator :: (YesodPersist site, YesodPersistBackend site ~ SqlPersistT) => Text -> Text -> KeyBackend SqlBackend User -> HandlerT site IO ()
requireModerator message project_handle user_id = do
    [ Value c :: Value Int ] <- runDB $ select $ from $ \ (pur `InnerJoin` project) -> do
        on_ $ pur ^. ProjectUserRoleProject ==. project ^. ProjectId
        where_ $ pur ^. ProjectUserRoleUser ==. val user_id
                &&. pur ^. ProjectUserRoleRole ==. val Moderator
                &&. project ^. ProjectHandle ==. val project_handle

        return $ count $ pur ^. ProjectUserRoleId

    when (c < 1) $ permissionDenied message


renderComment :: UserId -> [Role] -> Text -> Text -> M.Map UserId (Entity User) -> Int -> Int
    -> [CommentRetraction] -> M.Map CommentId CommentRetraction -> Bool -> Map TagId Tag -> Tree (Entity Comment) -> Maybe Widget -> Widget

renderComment viewer_id viewer_roles project_handle target users max_depth depth earlier_retractions retraction_map show_actions tag_map tree mcomment_form = do
    maybe_route <- handlerToWidget getCurrentRoute
    (comment_form, _) <- handlerToWidget $ generateFormPost $ commentForm Nothing Nothing

    let Entity comment_id comment = rootLabel tree
        children = subForest tree

        Entity user_id user = users M.! commentUser comment
        author_name = userPrintName $ Entity user_id user
        comment_time = renderTime $ commentCreatedTs comment
        unapproved = not . isJust $ commentModeratedTs comment

        top_level = commentDepth comment == 0
        even_depth = not top_level && commentDepth comment `mod` 2 == 1
        odd_depth = not top_level && not even_depth

        maybe_retraction = M.lookup comment_id retraction_map
        empty_list = []

        user_is_mod = Moderator `elem` viewer_roles
        can_rethread = user_id == viewer_id || user_is_mod

    tags <- fmap (L.sortBy (compare `on` atName)) $ handlerToWidget $ do
        comment_tags <- runDB $ select $ from $ \ comment_tag -> do
            where_ $ comment_tag ^. CommentTagComment ==. val comment_id
            return comment_tag

        annotateCommentTags tag_map project_handle target comment_id $ map entityVal comment_tags

    $(widgetFile "comment_body")


disabledCommentForm :: Form Markdown
disabledCommentForm = renderBootstrap3 $ areq snowdriftMarkdownField ("Reply" { fsAttrs = [("disabled",""), ("class","form-control")] }) Nothing

commentForm :: Maybe CommentId -> Maybe Markdown -> Form (Maybe CommentId, Markdown)
commentForm parent content = renderBootstrap3
    $ (,)
        <$> aopt' hiddenField "" (Just parent)
        <*> areq' snowdriftMarkdownField (if isJust parent then "Reply" else "Comment") content


{- TODO: Split out the core of this and move the wiki-specific stuff into Wiki -}


checkApproveComment :: Text -> Text -> CommentId -> Handler UserId
checkApproveComment project_handle target comment_id = do
    user_id <- requireAuthId

    (_, Entity page_id _) <- getPageInfo project_handle target
    checkCommentPage comment_id page_id
    requireModerator "You must be a moderator to approve posts." project_handle user_id

    return user_id
    

getOldApproveWikiCommentR :: Text -> Text -> CommentId -> Handler Html
getOldApproveWikiCommentR project_handle target comment_id = redirect $ ApproveWikiCommentR project_handle target comment_id

getApproveWikiCommentR :: Text -> Text -> CommentId -> Handler Html
getApproveWikiCommentR project_handle target comment_id = do
    void $ checkApproveComment project_handle target comment_id

    defaultLayout [whamlet|
        <form method="POST">
            <input type=submit value="approve post">
    |]


postOldApproveWikiCommentR :: Text -> Text -> CommentId -> Handler Html
postOldApproveWikiCommentR = postApproveWikiCommentR

postApproveWikiCommentR :: Text -> Text -> CommentId -> Handler Html
postApproveWikiCommentR project_handle target comment_id = do
    user_id <- checkApproveComment project_handle target comment_id

    now <- liftIO getCurrentTime

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



countReplies :: [Tree a] -> Int
countReplies = sum . map (F.sum . fmap (const 1))


checkRetractComment :: Text -> Text -> CommentId -> Handler (Entity User, ProjectId, Comment)
checkRetractComment project_handle target comment_id = do
    user_entity <- requireAuth
    comment <- runDB $ get404 comment_id

    (Entity project_id _, _) <- getPageInfo project_handle target

    when (commentUser comment /= entityKey user_entity) $ permissionDenied "You can only retract your own comments."

    return (user_entity, project_id, comment)


getOldRetractWikiCommentR :: Text -> Text -> CommentId -> Handler Html
getOldRetractWikiCommentR project_handle target comment_id = redirect $ RetractWikiCommentR project_handle target comment_id

getRetractWikiCommentR :: Text -> Text -> CommentId -> Handler Html
getRetractWikiCommentR project_handle target comment_id = do
    (Entity user_id user, project_id, comment) <- checkRetractComment project_handle target comment_id

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

    tags <- getTags comment_id

    let tag_map = M.fromList $ entityPairs tags

    (retract_form, _) <- generateFormPost $ retractForm Nothing

    roles <- getRoles user_id project_id

    let rendered_comment = renderDiscussComment user_id roles project_handle target False (return ()) (Entity comment_id comment) [] (M.singleton user_id $ Entity user_id user) earlier_retractions M.empty False tag_map

    defaultLayout $ [whamlet|
        ^{rendered_comment}
        <form method="POST">
            ^{retract_form}
            <input type="submit" name="mode" value="preview retraction">
    |]


postOldRetractWikiCommentR :: Text -> Text -> CommentId -> Handler Html
postOldRetractWikiCommentR = postRetractWikiCommentR

postRetractWikiCommentR :: Text -> Text -> CommentId -> Handler Html
postRetractWikiCommentR project_handle target comment_id = do
    (Entity user_id user, project_id, comment) <- checkRetractComment project_handle target comment_id

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

                    tags <- getTags comment_id

                    let tag_map = M.fromList $ entityPairs tags
                        soon = UTCTime (ModifiedJulianDay 0) 0
                        retraction = CommentRetraction soon reason comment_id
                        comment_entity = Entity comment_id comment
                        users = M.singleton user_id $ Entity user_id user
                        retractions = M.singleton comment_id retraction

                    roles <- getRoles user_id project_id

                    defaultLayout $ renderPreview form action $ renderDiscussComment user_id roles project_handle target False (return ()) comment_entity [] users earlier_retractions retractions False tag_map


                Just a | a == action -> do
                    now <- liftIO getCurrentTime
                    runDB $ insert_ $ CommentRetraction now reason comment_id

                    redirect $ DiscussCommentR project_handle target comment_id

                _ -> error "Error: unrecognized mode."
        _ -> error "Error when submitting form."


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
    (Entity project_id project, Entity page_id page) <- getPageInfo project_handle target

    affiliated <- runDB $ (||)
            <$> isProjectAffiliated project_handle user_id
            <*> isProjectAdmin "snowdrift" user_id

    moderator <- runDB $ isProjectModerator project_handle user_id

    (roots, rest, users, retraction_map) <- runDB $ do
        roots <- select $ from $ \ comment -> do
            where_ $ foldl1 (&&.) $ catMaybes
                [ Just $ comment ^. CommentDiscussion ==. val (wikiPageDiscussion page)
                , Just $ isNothing $ comment ^. CommentParent
                , if moderator then Nothing else Just $ not_ $ isNothing $ comment ^. CommentModeratedTs
                ]

            orderBy [asc (comment ^. CommentCreatedTs)]
            return comment

        rest <- select $ from $ \ comment -> do
            where_ $ foldl1 (&&.) $ catMaybes
                [ Just $ comment ^. CommentDiscussion ==. val (wikiPageDiscussion page)
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

    tags <- runDB $ select $ from return

    roles <- getRoles user_id project_id

    let tag_map = M.fromList $ entityPairs tags
        comments = forM_ roots $ \ root ->
            renderComment user_id roles project_handle target users 10 0 [] retraction_map True tag_map (buildCommentTree root rest) Nothing

    (comment_form, _) <- generateFormPost $ commentForm Nothing Nothing

    let has_comments = not $ null roots

    defaultLayout $ do
        setTitle . toHtml $ projectName project <> " Wiki Discussion - " <> target <> " | Snowdrift.coop"
        $(widgetFile "wiki_discuss")


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

    (Entity project_id _, Entity page_id page) <- getPageInfo project_handle target

    (root, rest, users, earlier_retractions, retraction_map) <- runDB $ do
        root <- get404 comment_id
        root_wiki_page_id <- getCommentPageId comment_id

        when (root_wiki_page_id /= page_id) $ error "Selected comment does not match selected page"

        subtree <- select $ from $ \ comment -> do
            where_ ( comment ^. CommentAncestorAncestor ==. val comment_id )
            return comment

        rest <- select $ from $ \ c -> do
            where_ $ c ^. CommentDiscussion ==. val (wikiPageDiscussion page)
                    &&. c ^. CommentId >. val comment_id
                    &&. c ^. CommentId `in_` valList (map (commentAncestorComment . entityVal) subtree)
            orderBy [asc (c ^. CommentParent), asc (c ^. CommentCreatedTs)]
            return c

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

    tags <- runDB $ select $ from return

    let tag_map = M.fromList $ entityPairs tags

    roles <- getRoles viewer_id project_id

    defaultLayout $ renderDiscussComment viewer_id roles project_handle target show_reply comment_form (Entity comment_id root) rest users earlier_retractions retraction_map True tag_map


renderDiscussComment :: UserId -> [Role] -> Text -> Text -> Bool -> Widget
    -> Entity Comment -> [Entity Comment]
    -> M.Map UserId (Entity User)
    -> [CommentRetraction]
    -> M.Map CommentId CommentRetraction
    -> Bool -> M.Map TagId Tag -> Widget

renderDiscussComment viewer_id roles project_handle target show_reply comment_form root rest users earlier_retractions retraction_map show_actions tag_map = do
    let tree = buildCommentTree root rest
        comment = renderComment viewer_id roles project_handle target users 1 0 earlier_retractions retraction_map show_actions tag_map tree mcomment_form
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

    (Entity project_id _, Entity _ page) <- getPageInfo project_handle target

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

                    tags <- runDB $ select $ from return

                    let tag_map = M.fromList $ entityPairs tags

                    (form, _) <- generateFormPost $ commentForm maybe_parent_id (Just text)

                    roles <- getRoles user_id project_id

                    let comment = Entity (Key $ PersistInt64 0) $ Comment now Nothing Nothing (wikiPageDiscussion page) maybe_parent_id user_id text depth
                        user_map = M.singleton user_id $ Entity user_id user
                        rendered_comment = renderDiscussComment user_id roles project_handle target False (return ()) comment [] user_map earlier_retractions M.empty False tag_map

                    defaultLayout $ renderPreview form action rendered_comment


                Just x | x == action -> do
                    runDB $ do
                        comment_id <- insert $ Comment now
                            (if established then Just now else Nothing)
                            (if established then Just user_id else Nothing)
                            (wikiPageDiscussion page)
                            maybe_parent_id user_id text depth

                        let content = T.lines $ (\ (Markdown str) -> str) text
                            tickets = map T.strip $ mapMaybe (T.stripPrefix "ticket:") content
                            tags = map T.strip $ mconcat $ map (T.splitOn ",") $ mapMaybe (T.stripPrefix "tags:") content

                        forM_ tickets $ \ ticket -> insert_ $ Ticket now now ticket comment_id
                        forM_ tags $ \ tag -> do
                            tag_id <- fmap (either entityKey id) $ insertBy $ Tag tag
                            insert_ $ CommentTag comment_id tag_id user_id 1

                        let getParentAncestors parent_id = do
                                comment_ancestor_entities <- select $ from $ \ comment_ancestor -> do
                                    where_ ( comment_ancestor ^. CommentAncestorComment ==. val parent_id )
                                    return comment_ancestor

                                let ancestors = map (commentAncestorAncestor . entityVal) comment_ancestor_entities
                                return $ parent_id : ancestors

                        ancestors <- maybe (return []) getParentAncestors maybe_parent_id

                        forM_ ancestors $ \ ancestor_id -> insert_ $ CommentAncestor comment_id ancestor_id

                        let selectAncestors = subList_select $ from $ \ ancestor -> do
                            where_ $ ancestor ^. CommentAncestorComment ==. val comment_id
                            return $ ancestor ^. CommentAncestorAncestor

                        update $ \ ticket -> do
                            set ticket [ TicketUpdatedTs =. val now ]
                            where_ $ ticket ^. TicketComment `in_` selectAncestors


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

    (_, Entity page_id page) <- getPageInfo project_handle target

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
    Entity project_id project <- runDB $ getBy404 $ UniqueProjectHandle project_handle

    req <- getRequest
    maybe_since <- lookupGetParam "since"
    since :: UTCTime <- case maybe_since of
        Nothing -> do
            viewtimes :: [Entity ViewTime] <- runDB $ select $ from $ \ viewtime -> do
                    where_ $
                        ( viewtime ^. ViewTimeUser ==. val viewer_id ) &&.
                        ( viewtime ^. ViewTimeProject ==. val project_id ) &&.
                        ( viewtime ^. ViewTimeType ==. val ViewComments )
                    return viewtime

            let comments_ts = case viewtimes of
                    [] -> userReadComments viewer
                    Entity _ viewtime : _ -> viewTimeTime viewtime

            redirectParams (WikiNewCommentsR project_handle) $ (T.pack "since", T.pack $ show comments_ts) : reqGetParams req
        Just since -> return (read . T.unpack $ since)

    maybe_from <- fmap (Key . PersistInt64 . read . T.unpack) <$> lookupGetParam "from"

    now <- liftIO getCurrentTime

    tags <- runDB $ select $ from return

    let tag_map = M.fromList $ entityPairs tags

    (new_comments, old_comments, pages, users, retraction_map) <- runDB $ do
        unfiltered_pages <- select $ from $ \ page -> do
            where_ $ page ^. WikiPageProject ==. val project_id
            return page

        let pages = M.fromList $ map (entityKey &&& entityVal) $ {- TODO filter ((userRole viewer >=) . wikiPageCanViewMeta . entityVal) -} unfiltered_pages


        let apply_offset comment = maybe id (\ from_comment rest -> comment ^. CommentId <=. val from_comment &&. rest) maybe_from
{-
        viewtimes :: [Entity ViewTime] <- select $ from $ \ viewtime -> do
            where_ $
                ( viewtime ^. ViewTimeUser ==. val viewer_id ) &&.
                ( viewtime ^. ViewTimeProject ==. val project_id ) &&.
                ( viewtime ^. ViewTimeType ==. val ViewComments )
            return viewtime

        let comments_ts = case viewtimes of
                [] -> userReadComments viewer
                (Entity _ viewtime):_ -> viewTimeTime viewtime
-}
        new_comments :: [Entity Comment] <- select $ from $ \ (comment `InnerJoin` wiki_page) -> do
            on_ $ comment ^. CommentDiscussion ==. wiki_page ^. WikiPageDiscussion
            where_ $ apply_offset comment (wiki_page ^. WikiPageId `in_` valList (M.keys pages))
                    &&. (comment ^. CommentCreatedTs >=. val since)

            orderBy [ desc (comment ^. CommentId) ]
            limit 51
            return comment

        old_comments :: [Entity Comment] <- select $ from $ \ (comment `InnerJoin` wiki_page) -> do
            on_ $ comment ^. CommentDiscussion ==. wiki_page ^. WikiPageDiscussion
            where_ $ apply_offset comment (wiki_page ^. WikiPageId `in_` valList (M.keys pages))
                    &&. (comment ^. CommentCreatedTs <. val since)
            orderBy [ desc (comment ^. CommentId) ]
            limit $ fromIntegral $ 51 - length new_comments
            --offset $ fromIntegral $ length new_comments
            return comment

        let user_ids = S.toList $ S.fromList $ map (commentUser . entityVal) (new_comments <> old_comments)
        users <- fmap (M.fromList . map (entityKey &&& id)) $ select $ from $ \ user -> do
            where_ ( user ^. UserId `in_` valList user_ids )
            return user

        retraction_map <- do
            retractions <- select $ from $ \ comment_retraction -> do
                where_ ( comment_retraction ^. CommentRetractionComment `in_` valList (map entityKey (new_comments <> old_comments) ) )
                return comment_retraction

            return . M.fromList . map ((commentRetractionComment &&& id) . entityVal) $ retractions

        return (new_comments, old_comments, pages, users, retraction_map)

    roles <- getRoles viewer_id project_id
    let new_comments' = take 50 new_comments
        old_comments' = take (50 - length new_comments') old_comments
        PersistInt64 to = unKey $ minimum (map entityKey (new_comments' <> old_comments') )
        render_comments comments =
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

                [Value target] <- handlerToWidget $ runDB $ select $ from $ \ (c `InnerJoin` p) -> do
                    on_ $ p ^. WikiPageDiscussion ==. c ^. CommentDiscussion
                    where_ $ c ^. CommentId ==. val comment_id
                    return $ p ^. WikiPageTarget

                let rendered_comment = renderComment viewer_id roles project_handle target users 1 0 earlier_retractions retraction_map True tag_map (Node (Entity comment_id comment) []) Nothing

                [whamlet|$newline never
                    <div .row>
                        <div .col-md-9 .col-md-offset-1 .col-lg-8 .col-lg-offset-2>
                            On #
                            <a href="@{WikiR project_handle target}">
                                #{target}
                            :
                            ^{rendered_comment}
                |]
        rendered_new_comments = render_comments new_comments'
        rendered_old_comments = render_comments old_comments'
        show_older = (length new_comments + length old_comments) > 50

    runDB $ do
        c <- updateCount $ \ viewtime -> do
                set viewtime [ ViewTimeTime =. val now ]
                where_ $
                    ( viewtime ^. ViewTimeUser ==. val viewer_id ) &&.
                    ( viewtime ^. ViewTimeProject ==. val project_id ) &&.
                    ( viewtime ^. ViewTimeType ==. val ViewComments )

        when (c == 0) $ insert_ $ ViewTime viewer_id project_id ViewComments now

    defaultLayout $ do
        setTitle . toHtml $ projectName project <> " - New Comments | Snowdrift.coop"
        $(widgetFile "wiki_new_comments")

rethreadForm :: Form (Text, Text)
rethreadForm = renderBootstrap3 $ (,)
    <$> areq' textField "New Parent Url" Nothing
    <*> areq' textField "Reason" Nothing

getRethreadWikiCommentR :: Text -> Text -> CommentId -> Handler Html
getRethreadWikiCommentR project_handle target comment_id = do
    (form, _) <- generateFormPost rethreadForm

    defaultLayout $(widgetFile "rethread")


postRethreadWikiCommentR :: Text -> Text -> CommentId -> Handler Html
postRethreadWikiCommentR project_handle target comment_id = do
    user_id <- requireAuthId

    Entity project_id _ <- runDB $ getBy404 $ UniqueProjectHandle project_handle

    do
        [ Value c :: Value Int ] <- runDB $ select $ from $ \ pur -> do
            where_ $ pur ^. ProjectUserRoleUser ==. val user_id
                &&. pur ^. ProjectUserRoleProject ==. val project_id
                &&. pur ^. ProjectUserRoleRole ==. val Moderator
            return $ count $ pur ^. ProjectUserRoleId

        when (c < 1) $ permissionDenied "You must be a moderator to rethread"

    comment <- runDB $ get404 comment_id

    [ Value page_count :: Value Int ] <- runDB $ select $ from $ \ page -> do
        where_ $ page ^. WikiPageTarget ==. val target
            &&. page ^. WikiPageDiscussion ==. val (commentDiscussion comment)
        return countRows

    when (page_count < 1) $ error "that comment doesn't belong to that page's discussion"

    ((result, _), _) <- runFormPost rethreadForm

    case result of
        FormSuccess (new_parent_url, reason) -> do
            app <- getYesod
            let splitPath = drop 1 . T.splitOn "/"
                stripQuery = fst . T.break (== '?')
                stripRoot = fromMaybe new_parent_url . T.stripPrefix (appRoot $ settings app)
                url = splitPath $ stripQuery $ stripRoot new_parent_url

            (new_parent_id, new_discussion_id) <- case parseRoute (url, []) of
                Just (DiscussCommentR new_project_handle new_target new_parent_id) -> do
                    Entity new_project_id _ <- getByErr "could not find project" $ UniqueProjectHandle new_project_handle

                    when (new_project_id /= project_id) $ requireModerator "You must be a moderator to rethread." new_project_handle user_id

                    maybe_new_page <- runDB $ getBy $ UniqueWikiTarget new_project_id new_target

                    let new_page = maybe (error "could not find new page") entityVal maybe_new_page

                    return (Just new_parent_id, wikiPageDiscussion new_page)

                Just (DiscussWikiR new_project_handle new_target) -> do
                    new_project_maybe <- runDB $ getBy $ UniqueProjectHandle new_project_handle
                    let new_project_id = maybe (error "could not find project") entityKey new_project_maybe

                    when (new_project_id /= project_id) $ requireModerator "You must be a moderator to rethread." new_project_handle user_id

                    maybe_new_page <- runDB $ getBy $ UniqueWikiTarget new_project_id new_target

                    let new_page = maybe (error "could not find new page") entityVal maybe_new_page

                    return (Nothing, wikiPageDiscussion new_page)

                Nothing -> error "failed to parse URL"

                _ -> error "could not find discussion for that URL"

            when (new_parent_id == commentParent comment && new_discussion_id == commentDiscussion comment) $ error "trying to move comment to its current location"

            mode <- lookupPostParam "mode"
            let action :: Text = "rethread"
            case mode of
                Just "preview" -> error "no preview for rethreads yet" -- TODO

                Just a | a == action -> do
                    now <- liftIO getCurrentTime

                    runDB $ do
                        insert_ $ CommentRethread now user_id (commentParent comment) (commentDiscussion comment) new_parent_id new_discussion_id comment_id reason

                        let getAncestors c = do
                                ancestors <- select $ from $ \ comment_ancestor -> do
                                    where_ $ comment_ancestor ^. CommentAncestorComment ==. val c
                                    return $ comment_ancestor ^. CommentAncestorAncestor

                                return $ c : map (\ (Value x) -> x) ancestors

                        old_ancestors <- maybe (return []) getAncestors $ commentParent comment
                        new_ancestors <- maybe (return []) getAncestors new_parent_id

                        descendents' <- select $ from $ \ comment_ancestor -> do
                                where_ $ comment_ancestor ^. CommentAncestorAncestor ==. val comment_id
                                return $ comment_ancestor ^. CommentAncestorComment

                        let descendents = comment_id : map (\ (Value x) -> x) descendents'

                        unless (null old_ancestors) $ do
                            to_delete <- select $ from $ \ comment_ancestor -> do
                                where_ $ comment_ancestor ^. CommentAncestorComment `in_` valList old_ancestors
                                        &&. comment_ancestor ^. CommentAncestorAncestor `in_` valList descendents
                                return comment_ancestor

                            liftIO $ print to_delete

                            delete $ from $ \ comment_ancestor -> do
                                where_ $ comment_ancestor ^. CommentAncestorComment `in_` valList old_ancestors
                                        &&. comment_ancestor ^. CommentAncestorAncestor `in_` valList descendents

                            update $ \ c -> do
                                where_ $ c ^. CommentId ==. val comment_id
                                set c [ CommentParent =. val new_parent_id ]

                        forM_ new_ancestors $ \ new_ancestor_id -> forM_ descendents $ \ descendent -> do
                            liftIO $ putStrLn $ "inserting comment ancestor " ++ show descendent ++ " " ++ show new_ancestor_id
                            insert_ $ CommentAncestor descendent new_ancestor_id

                        when (new_discussion_id /= commentDiscussion comment) $ update $ \ c -> do
                                where_ $ c ^. CommentId `in_` valList descendents
                                set c [ CommentDiscussion =. val new_discussion_id ]

                    redirect new_parent_url

                _ -> error "Error: unrecognized mode."
        _ -> error "Error when submitting form."
