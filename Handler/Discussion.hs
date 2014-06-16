{-# LANGUAGE TupleSections #-}

module Handler.Discussion where

import Import

import Data.Tree

import qualified Data.Foldable as F
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Text as T

import Model.AnnotatedTag
import Model.User
import Model.Role
import Model.ClosureType
import Model.CollapseState
import Model.ViewType
import Model.WikiPage

import Widgets.Markdown
import Widgets.Preview
import Widgets.Tag
import Widgets.Time

import Yesod.Markdown
import Model.Markdown

import Yesod.Default.Config

import Network.HTTP.Types.Status

import qualified Control.Monad.State as St


getTags :: CommentId -> Handler [Entity Tag]
getTags comment_id = runDB $ select $ from $ \ (comment_tag `InnerJoin` tag) -> do
    on_ $ comment_tag ^. CommentTagTag ==. tag ^. TagId
    where_ $ comment_tag ^. CommentTagComment ==. val comment_id
    return tag


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

-- |renderComment is for how each comment is rendered within whatever larger context it may have
renderComment :: UTCTime -> Maybe (Entity User) -> [Role] -> Text -> Text -> M.Map UserId (Entity User) -> Int -> Int
    -> [CommentClosure] -> M.Map CommentId CommentClosure -> Bool -> Map TagId Tag -> Tree (Entity Comment) -> Maybe Widget -> Widget

renderComment now mviewer viewer_roles project_handle target users max_depth depth earlier_closures closure_map show_actions tag_map tree mcomment_form = do
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

        maybe_closure = M.lookup comment_id closure_map
        empty_list = []

        user_is_mod = Moderator `elem` viewer_roles
        can_rethread = maybe False (\ (Entity viewer_id _) -> user_id == viewer_id || user_is_mod) mviewer

        -- TODO unify these with the checks in the handlers
        can_retract = maybe False (\ (Entity viewer_id _) -> user_id == viewer_id) mviewer
        can_close = maybe False (\ (Entity _ viewer) -> isJust (userEstablishedTs viewer)) mviewer

    tags <- fmap (L.sortBy (compare `on` atName)) $ handlerToWidget $ do
        comment_tags <- runDB $ select $ from $ \ comment_tag -> do
            where_ $ comment_tag ^. CommentTagComment ==. val comment_id
            return comment_tag

        annotateCommentTags tag_map project_handle target comment_id $ map entityVal comment_tags

    Just action <- getCurrentRoute

    collapse_state <- return FullyVisible -- maybe (return FullyVisible) (handlerToWidget . collapseState now) maybe_closure

    case (depth == 0, collapse_state) of
        (True, _) -> $(widgetFile "comment_body")
        (_, FullyVisible) -> $(widgetFile "comment_body")

        (_, Collapsed) -> -- TODO: prettify, unify with messages in comment_body
            [whamlet|
                $case maybe Closed commentClosureType maybe_closure
                    $of Closed
                        <div .closed>
                            Closed #
                            <a href=@{DiscussCommentR project_handle target comment_id}>
                                comment thread
                            \ collapsed.


                    $of Retracted
                        <div .retracted>
                            Retracted #
                            <a href=@{DiscussCommentR project_handle target comment_id}>
                                comment thread
                            \ collapsed.
            |]

        (_, FullyHidden) -> return ()


disabledCommentForm :: Form Markdown
disabledCommentForm = renderBootstrap3 $ areq snowdriftMarkdownField ("Reply" { fsAttrs = [("disabled",""), ("class","form-control")] }) Nothing

commentForm :: Maybe CommentId -> Maybe Markdown -> Form Markdown
commentForm parent content =
    let comment_label = if isJust parent then "Reply" else "New Topic"
     in renderBootstrap3 $ areq' snowdriftMarkdownField comment_label content


{- TODO: Split out the core of this and move the wiki-specific stuff into Wiki -}


checkApproveComment :: Text -> Text -> CommentId -> Handler UserId
checkApproveComment project_handle target comment_id = do
    user_id <- requireAuthId

    (_, Entity page_id _) <- getPageInfo project_handle target
    checkCommentPage comment_id page_id
    requireModerator "You must be a moderator to approve posts." project_handle user_id

    return user_id


getApproveWikiCommentR :: Text -> Text -> CommentId -> Handler Html
getApproveWikiCommentR project_handle target comment_id = do
    void $ checkApproveComment project_handle target comment_id

    defaultLayout [whamlet|
        <form method="POST">
            <input type=submit value="approve post">
    |]


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


closureForm :: ClosureType -> Maybe Markdown -> Form Markdown
closureForm Retracted reason = renderBootstrap3 $ areq snowdriftMarkdownField "Retraction reason:" reason
closureForm Closed reason = renderBootstrap3 $ areq snowdriftMarkdownField "Reason for closing:" reason


countReplies :: [Tree a] -> Int
countReplies = sum . map (F.sum . fmap (const 1))


checkRetractComment :: UserId -> Text -> Text -> CommentId -> Handler (ProjectId, Comment)
checkRetractComment user_id project_handle target comment_id = do
    comment <- runDB $ get404 comment_id

    (Entity project_id _, _) <- getPageInfo project_handle target

    when (commentUser comment /= user_id) $ permissionDenied "You can only retract your own comments."

    return (project_id, comment)


checkCloseComment :: User -> Text -> Text -> CommentId -> Handler (ProjectId, Comment)
checkCloseComment user project_handle target comment_id = do
    comment <- runDB $ get404 comment_id

    (Entity project_id _, _) <- getPageInfo project_handle target

    -- TODO: what should this be?
    -- Aaron says: I think we should allow established to mark as closed,
    -- but only *affiliated* OR the original poster should do so in one step,
    -- otherwise, the marking of closed should require *moderator* confirmation…
    -- We should also have a re-open function.
    -- There are now comments discussing these things on the site.
    unless (isJust $ userEstablishedTs user) $ permissionDenied "You must be an established user to close a conversation."

    return (project_id, comment)


getRetractWikiCommentR :: Text -> Text -> CommentId -> Handler Html
getRetractWikiCommentR = getCloseWikiComment Retracted

getCloseWikiCommentR :: Text -> Text -> CommentId -> Handler Html
getCloseWikiCommentR = getCloseWikiComment Closed

getCloseWikiComment :: ClosureType -> Text -> Text -> CommentId -> Handler Html
getCloseWikiComment closure_type project_handle target comment_id = do
    Entity user_id user <- requireAuth

    (project_id, comment) <- case closure_type of
        Retracted -> checkRetractComment user_id project_handle target comment_id
        Closed -> checkCloseComment user project_handle target comment_id

    earlier_closures <- runDB $
        case commentParent comment of
            Just parent_id -> do
                ancestors <- do
                    comment_ancestor_entities <- select $ from $ \ comment_ancestor -> do
                        where_ ( comment_ancestor ^. CommentAncestorComment ==. val parent_id )
                        return comment_ancestor

                    return . (parent_id :) . map (commentAncestorAncestor . entityVal) $ comment_ancestor_entities

                fmap (map entityVal) $ select $ from $ \ closure -> do
                    where_ ( closure ^. CommentClosureComment `in_` valList ancestors )
                    return closure

            Nothing -> return []

    tags <- getTags comment_id

    let tag_map = M.fromList $ entityPairs tags

    let poster_id = commentUser comment
    poster <- runDB $ get404 poster_id
    let users = (M.fromList [ (user_id, Entity user_id user), (poster_id, Entity poster_id poster) ])

    (closure_form, _) <- generateFormPost $ closureForm closure_type Nothing

    roles <- getRoles user_id project_id

    let rendered_comment = renderDiscussComment (Just $ Entity user_id user) roles project_handle target False (return ()) (Entity comment_id comment) [] users earlier_closures M.empty False tag_map

    defaultLayout $ [whamlet|
        ^{rendered_comment}
        <form method="POST">
            ^{closure_form}
            <input type="submit" name="mode" value="preview">
    |]


postRetractWikiCommentR :: Text -> Text -> CommentId -> Handler Html
postRetractWikiCommentR = postCloseWikiComment Retracted

postCloseWikiCommentR :: Text -> Text -> CommentId -> Handler Html
postCloseWikiCommentR = postCloseWikiComment Closed

-- a *lot* of postCloseWikiComment is completely redundant to getCloseWikiComment above. There's gotta be a way to clean this up.
postCloseWikiComment :: ClosureType -> Text -> Text -> CommentId -> Handler Html
postCloseWikiComment closure_type project_handle target comment_id = do
    Entity user_id user <- requireAuth

    (project_id, comment) <- case closure_type of
        Retracted -> checkRetractComment user_id project_handle target comment_id
        Closed -> checkCloseComment user project_handle target comment_id

    ((result, _), _) <- runFormPost $ closureForm closure_type Nothing

    case result of
        FormSuccess reason -> do
            earlier_closures <- runDB $
                case commentParent comment of
                    Just parent_id -> do
                        ancestors <- do
                            comment_ancestor_entities <- select $ from $ \ comment_ancestor -> do
                                where_ ( comment_ancestor ^. CommentAncestorComment ==. val parent_id )
                                return comment_ancestor

                            return . (parent_id :) . map (commentAncestorAncestor . entityVal) $ comment_ancestor_entities
                        map entityVal <$> selectList [ CommentClosureComment <-. ancestors ] []

                    Nothing -> return []

            let action :: Text = case closure_type of
                    Closed -> "close"
                    Retracted -> "retract"

            mode <- lookupPostParam "mode"
            case mode of
                Just "preview" -> do
                    soon <- liftIO getCurrentTime

                    (form, _) <- generateFormPost $ closureForm closure_type (Just reason)

                    tags <- getTags comment_id

                    let tag_map = M.fromList $ entityPairs tags
                        closure = CommentClosure soon user_id closure_type reason comment_id
                        closures = M.singleton comment_id closure

                    let poster_id = commentUser comment
                    poster <- runDB $ get404 poster_id
                    let users = (M.fromList [ (user_id, Entity user_id user), (poster_id, Entity poster_id poster) ])

                    roles <- getRoles user_id project_id

                    defaultLayout $ renderPreview form action $
                        renderDiscussComment (Just $ Entity user_id user) roles project_handle target False (return ()) (Entity comment_id comment) [] users earlier_closures closures False tag_map


                Just a | a == action -> do
                    now <- liftIO getCurrentTime
                    runDB $ insert_ $ CommentClosure now user_id closure_type reason comment_id

                    redirect $ DiscussCommentR project_handle target comment_id

                m -> error $ "Error: unrecognized mode (" ++ show m ++ ")"
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


-- |getDiscussWikiR generates the associated discussion page for each wiki page
getDiscussWikiR :: Text -> Text -> Handler Html
getDiscussWikiR project_handle target = do
    muser <- maybeAuth
    (Entity project_id project, Entity page_id page) <- getPageInfo project_handle target

    now <- liftIO getCurrentTime

    affiliated <- case muser of
        Nothing -> return False
        Just (Entity user_id _) ->
            runDB $ (||)
                <$> isProjectAffiliated project_handle user_id
                <*> isProjectAdmin "snowdrift" user_id

    moderator <- case muser of
        Nothing -> return False
        Just (Entity user_id _) ->
            runDB $ isProjectModerator project_handle user_id

    (roots, rest, users, closure_map) <- runDB $ do
        roots <- select $ from $ \ comment -> do
            where_ $ foldl1 (&&.) $ catMaybes
                [ Just $ comment ^. CommentDiscussion ==. val (wikiPageDiscussion page)
                , Just $ isNothing $ comment ^. CommentParent
                , Just $ isNothing $ comment ^. CommentRethreaded
                , if moderator then Nothing else Just $ not_ $ isNothing $ comment ^. CommentModeratedTs
                ]

            orderBy [asc (comment ^. CommentCreatedTs)]
            return comment

        rest <- select $ from $ \ comment -> do
            where_ $ foldl1 (&&.) $ catMaybes
                [ Just $ comment ^. CommentDiscussion ==. val (wikiPageDiscussion page)
                , Just $ not_ $ isNothing $ comment ^. CommentParent
                , Just $ isNothing $ comment ^. CommentRethreaded
                , if moderator then Nothing else Just $ not_ $ isNothing $ comment ^. CommentModeratedTs
                ]

            orderBy [asc (comment ^. CommentParent), asc (comment ^. CommentCreatedTs)]
            return comment

        let get_user_ids = S.fromList . map (commentUser . entityVal) . F.toList
            user_id_list = S.toList $ get_user_ids roots `S.union` get_user_ids rest

        user_entities <- selectList [ UserId <-. user_id_list ] []

        let users = M.fromList $ map (entityKey &&& id) user_entities

        closure_map <- M.fromList . map ((commentClosureComment &&& id) . entityVal) <$>
            selectList [ CommentClosureComment <-. map entityKey (roots ++ rest) ] []

        return (roots, rest, users, closure_map)

    tags <- runDB $ select $ from return

    roles <- case muser of
        Nothing -> return []
        Just (Entity user_id _) -> getRoles user_id project_id

    let tag_map = M.fromList $ entityPairs tags
        comments = forM_ roots $ \ root ->
            renderComment now muser roles project_handle target users 8 0 [] closure_map True tag_map (buildCommentTree root rest) Nothing

    (comment_form, _) <- generateFormPost $ commentForm Nothing Nothing

    let has_comments = not $ null roots

    defaultLayout $ do
        setTitle . toHtml $ projectName project <> " Wiki Discussion - " <> target <> " | Snowdrift.coop"
        $(widgetFile "wiki_discuss")

-- This is just because we used to have "/comment/#" with that long thing,
-- and this keeps any permalinks from breaking
getOldDiscussCommentR :: Text -> Text -> CommentId -> Handler Html
getOldDiscussCommentR project_handle target comment_id = redirect $ DiscussCommentR project_handle target comment_id

-- This is a hacked change where getDiscussCommentR' below should really be
-- adapted to become this and have only one function.
getDiscussCommentR :: Text -> Text -> CommentId -> Handler Html
getDiscussCommentR =
    getDiscussCommentR' False

getReplyCommentR :: Text -> Text -> CommentId -> Handler Html
getReplyCommentR a b c = do
    _ <- requireAuth
    getDiscussCommentR' True a b c

postReplyCommentR :: Text -> Text -> CommentId -> Handler Html
postReplyCommentR project_handle target comment_id = do
    (project_entity, Entity _ page) <- getPageInfo project_handle target

    ((result, _), _) <- runFormPost $ commentForm (Just comment_id) Nothing

    case result of
        FormSuccess text -> processWikiComment (Just comment_id) text project_entity page
        FormMissing -> error "Form missing."
        FormFailure msgs -> error $ "Error submitting form: " ++ T.unpack (T.intercalate "\n" msgs)


getDiscussCommentR' :: Bool -> Text -> Text -> CommentId -> Handler Html
getDiscussCommentR' show_reply project_handle target comment_id = do
    rethread <- runDB $ select $ from $ \ comment_rethread -> do
        where_ $ comment_rethread ^. CommentRethreadOldComment ==. val comment_id
        return $ comment_rethread ^. CommentRethreadNewComment


    case rethread of
        [] -> return ()
        Value destination_comment_id : _ -> do
            route <- runDB $ do
                -- TODO: any way to statically make sure we've covered all discussion types?
                destination_infos <- select $ from $ \ (comment `LeftOuterJoin` page) -> do
                    on_ $ (just $ comment ^. CommentDiscussion) ==. page ?. WikiPageDiscussion
                    where_ $ comment ^. CommentId ==. val destination_comment_id
                    return $ (page ?. WikiPageTarget)

                let route_destination [] = error "unrecognized discussion for comment"

                    route_destination (Value (Just destination_target) : _)=
                        let route = if show_reply then ReplyCommentR else DiscussCommentR
                         in route project_handle destination_target destination_comment_id

                    route_destination (_ : rest) = route_destination rest

                return $ route_destination destination_infos

            redirectWith movedPermanently301 route


    mviewer <- maybeAuth

    (Entity project_id _, Entity page_id page) <- getPageInfo project_handle target

    roles <- case mviewer of
        Nothing -> return []
        Just (Entity viewer_id _) -> getRoles viewer_id project_id

    moderator <- case mviewer of
        Nothing -> return False
        Just (Entity viewer_id _) ->
            runDB $ isProjectModerator project_handle viewer_id

    (root, rest, users, earlier_closures, closure_map) <- runDB $ do
        root <- get404 comment_id
        root_wiki_page_id <- getCommentPageId comment_id

        when (root_wiki_page_id /= page_id) $ error "Selected comment does not match selected page"

        subtree <- select $ from $ \ comment_ancestor -> do
            where_ $ comment_ancestor ^. CommentAncestorAncestor ==. val comment_id
            return comment_ancestor

        rest <- select $ from $ \ c -> do
            where_ $ foldl1 (&&.) $ catMaybes
                [ Just $ c ^. CommentDiscussion ==. val (wikiPageDiscussion page)
                , Just $ c ^. CommentId >. val comment_id
                , Just $ isNothing $ c ^. CommentRethreaded
                , Just $ c ^. CommentId `in_` valList (map (commentAncestorComment . entityVal) subtree)
                , if moderator then Nothing else Just $ not_ $ isNothing $ c ^. CommentModeratedTs
                ]
            orderBy [asc (c ^. CommentParent), asc (c ^. CommentCreatedTs)]
            return c

        let get_user_ids = S.fromList . map (commentUser . entityVal) . F.toList
            user_id_list = S.toList $ S.insert (commentUser root) $ get_user_ids rest

        user_entities <- select $ from $ \ user -> do
            where_ ( user ^. UserId `in_` valList user_id_list )
            return user

        let users = M.fromList $ map (entityKey &&& id) user_entities

        earlier_closures <- fmap (map entityVal) $ select $ from $ \ (comment_ancestor `InnerJoin` closure) -> do
            on_ (comment_ancestor ^. CommentAncestorAncestor ==. closure ^. CommentClosureComment)
            where_ ( comment_ancestor ^. CommentAncestorComment ==. val comment_id )
            return closure

        closure_map <- fmap (M.fromList . map ((commentClosureComment &&& id) . entityVal)) $ select $ from $ \ closure -> do
            where_ ( closure ^. CommentClosureComment `in_` valList (comment_id : map entityKey rest) )
            return closure

        return (root, rest, users, earlier_closures, closure_map)

    (comment_form, _) <- generateFormPost $ commentForm (Just comment_id) Nothing

    tags <- runDB $ select $ from return

    let tag_map = M.fromList $ entityPairs tags

    defaultLayout $ renderDiscussComment mviewer roles project_handle target show_reply comment_form (Entity comment_id root) rest users earlier_closures closure_map True tag_map

-- |renderDiscussComment is for permalink views of particular comments
renderDiscussComment :: Maybe (Entity User) -> [Role] -> Text -> Text -> Bool -> Widget
    -> Entity Comment -> [Entity Comment]
    -> M.Map UserId (Entity User)
    -> [CommentClosure]
    -> M.Map CommentId CommentClosure
    -> Bool -> M.Map TagId Tag -> Widget

renderDiscussComment viewer roles project_handle target show_reply comment_form root rest users earlier_closures closure_map show_actions tag_map = do
    now <- liftIO getCurrentTime

    let tree = buildCommentTree root rest
        comment = renderComment now viewer roles project_handle target users 11 0 earlier_closures closure_map show_actions tag_map tree mcomment_form
        mcomment_form =
            if show_reply
                then Just comment_form
                else Nothing

    $(widgetFile "comment")


processWikiComment :: Maybe CommentId -> Markdown -> Entity Project -> WikiPage -> Handler Html
processWikiComment maybe_parent_id text (Entity project_id project) page = do
    Entity user_id user <- requireAuth

    let established = isJust $ userEstablishedTs user

    now <- liftIO getCurrentTime

    depth <- case maybe_parent_id of
        Just parent_id -> do
            Just parent <- runDB $ get parent_id
            return $ (+1) $ commentDepth parent
        _ -> return 0

    mode <- lookupPostParam "mode"

    let action :: Text = "post"

    case mode of
        Just "preview" -> do
            earlier_closures <- runDB $
                case maybe_parent_id of
                    Just parent_id -> do
                        ancestors <- fmap ((parent_id :) . map (commentAncestorAncestor . entityVal)) $ select $ from $ \ ancestor -> do
                            where_ ( ancestor ^. CommentAncestorComment ==. val parent_id )
                            return ancestor

                        fmap (map entityVal) $ select $ from $ \ closure -> do
                            where_ ( closure ^. CommentClosureComment `in_` valList ancestors )
                            return closure

                    Nothing -> return []

            tags <- runDB $ select $ from return

            let tag_map = M.fromList $ entityPairs tags

            (form, _) <- generateFormPost $ commentForm maybe_parent_id (Just text)

            roles <- getRoles user_id project_id

            let comment = Entity (Key $ PersistInt64 0) $ Comment now Nothing Nothing Nothing (wikiPageDiscussion page) maybe_parent_id user_id text depth
                user_map = M.singleton user_id $ Entity user_id user
                rendered_comment = renderDiscussComment (Just $ Entity user_id user) roles (projectHandle project) (wikiPageTarget page) False (return ()) comment [] user_map earlier_closures M.empty False tag_map

            defaultLayout $ renderPreview form action rendered_comment


        Just x | x == action -> do
            maybe_parent_id' <- runDB $ do
                let getDestination comment_id = do
                        destination <- select $ from $ \ comment_rethread -> do
                            where_ $ comment_rethread ^. CommentRethreadOldComment ==. val comment_id
                            return $ comment_rethread ^. CommentRethreadNewComment

                        case destination of
                            [] -> return comment_id
                            Value comment_id' : _ -> getDestination comment_id'

                maybe_parent_id' <- maybe (return Nothing) (fmap Just . getDestination) maybe_parent_id

                comment_id <- insert $ Comment now
                    (if established then Just now else Nothing)
                    (if established then Just user_id else Nothing)
                    Nothing
                    (wikiPageDiscussion page)
                    maybe_parent_id' user_id text depth

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

                return maybe_parent_id'


            addAlert "success" $ if established then "comment posted" else "comment submitted for moderation"
            redirect $ maybe (DiscussWikiR (projectHandle project) (wikiPageTarget page)) (DiscussCommentR (projectHandle project) (wikiPageTarget page)) maybe_parent_id'

        m -> error $ "Error: unrecognized mode (" ++ show m ++ ")"


postDiscussWikiR :: Text -> Text -> Handler Html
postDiscussWikiR project_handle target = do
    (project_entity, Entity _ page) <- getPageInfo project_handle target

    ((result, _), _) <- runFormPost $ commentForm Nothing Nothing

    case result of
        FormSuccess text -> processWikiComment Nothing text project_entity page
        FormMissing -> error "Form missing."
        FormFailure msgs -> error $ "Error submitting form: " ++ T.unpack (T.intercalate "\n" msgs)


getNewDiscussWikiR :: Text -> Text -> Handler Html
getNewDiscussWikiR project_handle target = do
    Entity user_id user <- requireAuth
    let action = DiscussWikiR project_handle target

    (_, Entity page_id page) <- getPageInfo project_handle target

    affiliated <- runDB $ (||)
            <$> isProjectAffiliated project_handle user_id
            <*> isProjectAdmin "snowdrift" user_id

    (comment_form, _) <- generateFormPost $ commentForm Nothing Nothing

    defaultLayout $(widgetFile "wiki_discuss_new")


postNewDiscussWikiR :: Text -> Text -> Handler Html
postNewDiscussWikiR = postDiscussWikiR


getWikiNewCommentsR :: Text -> Handler Html
getWikiNewCommentsR project_handle = do
    mviewer <- maybeAuth
    Entity project_id project <- runDB $ getBy404 $ UniqueProjectHandle project_handle

    req <- getRequest
    maybe_since <- lookupGetParam "since"
    since :: UTCTime <- case mviewer of
        Nothing -> liftIO getCurrentTime
        Just (Entity viewer_id viewer) -> case maybe_since of
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

    (new_comments, old_comments, pages, users, closure_map) <- runDB $ do
        unfiltered_pages <- select $ from $ \ page -> do
            where_ $ page ^. WikiPageProject ==. val project_id
            return page

        let pages = M.fromList $ map (entityKey &&& entityVal) {- TODO filter ((userRole viewer >=) . wikiPageCanViewMeta . entityVal) -} unfiltered_pages


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

        closure_map <- do
            closures <- select $ from $ \ comment_closure -> do
                where_ ( comment_closure ^. CommentClosureComment `in_` valList (map entityKey (new_comments <> old_comments) ) )
                return comment_closure

            return . M.fromList . map ((commentClosureComment &&& id) . entityVal) $ closures

        return (new_comments, old_comments, pages, users, closure_map)

    roles <- case mviewer of
        Nothing -> return []
        Just (Entity viewer_id _) -> getRoles viewer_id project_id
    let new_comments' = take 50 new_comments
        old_comments' = take (50 - length new_comments') old_comments
        PersistInt64 to = unKey $ minimum (map entityKey (new_comments' <> old_comments') )
        render_comments comments =
            if null comments
             then [whamlet||]
             else forM_ comments $ \ (Entity comment_id comment) -> do
                earlier_closures <- handlerToWidget $ runDB $ do
                    ancestors <- select $ from $ \ comment_ancestor -> do
                        where_ ( comment_ancestor ^. CommentAncestorComment ==. val comment_id )
                        return comment_ancestor

                    fmap (map entityVal) $ select $ from $ \ comment_closure -> do
                        where_ ( comment_closure ^. CommentClosureComment `in_` valList (map (commentAncestorAncestor . entityVal) ancestors))
                        orderBy [ asc (comment_closure ^. CommentClosureComment) ]
                        return comment_closure

                [Value target] <- handlerToWidget $ runDB $ select $ from $ \ (c `InnerJoin` p) -> do
                    on_ $ p ^. WikiPageDiscussion ==. c ^. CommentDiscussion
                    where_ $ c ^. CommentId ==. val comment_id
                    return $ p ^. WikiPageTarget

                let rendered_comment = renderComment now mviewer roles project_handle target users 0 0 {- max_depth is irrelevant for the new-comments listing -}
                                           earlier_closures closure_map True tag_map (Node (Entity comment_id comment) []) Nothing

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

    case mviewer of
        Nothing -> return ()
        Just (Entity viewer_id _) ->
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


rethreadComments :: RethreadId -> Int -> Maybe CommentId -> DiscussionId -> [CommentId] -> SqlPersistT Handler [CommentId]

rethreadComments rethread_id depth_offset maybe_new_parent_id new_discussion_id comment_ids = do
    new_comment_ids <- flip St.evalStateT M.empty $ forM comment_ids $ \ comment_id -> do
        rethreads <- St.get

        Just comment <- get comment_id

        let new_parent_id = maybe maybe_new_parent_id Just $ M.lookup (commentParent comment) rethreads

        new_comment_id <- insert $ comment
            { commentDepth = commentDepth comment - depth_offset
            , commentParent = new_parent_id
            , commentDiscussion = new_discussion_id
            }

        St.put $ M.insert (Just comment_id) new_comment_id rethreads

        return new_comment_id

    forM_ (zip comment_ids new_comment_ids) $ \ (comment_id, new_comment_id) -> do
        update $ \ comment_tag -> do
            where_ $ comment_tag ^. CommentTagComment ==. val comment_id
            set comment_tag [ CommentTagComment =. val new_comment_id ]

        update $ \ ticket -> do
            where_ $ ticket ^. TicketComment ==. val comment_id
            set ticket [ TicketComment =. val new_comment_id ]

        insert_ $ CommentRethread rethread_id comment_id new_comment_id

    insertSelect $ from $ \ (comment_closure `InnerJoin` comment_rethread) -> do
        on_ $ comment_closure ^. CommentClosureComment ==. comment_rethread ^. CommentRethreadOldComment
        return $ CommentClosure
                    <#  (comment_closure ^. CommentClosureTs)
                    <&> (comment_closure ^. CommentClosureClosedBy)
                    <&> (comment_closure ^. CommentClosureType)
                    <&> (comment_closure ^. CommentClosureReason)
                    <&> (comment_rethread ^. CommentRethreadNewComment)

    update $ \ comment -> do
        where_ $ comment ^. CommentId `in_` valList comment_ids
        set comment [ CommentRethreaded =. just (val rethread_id) ]

    return new_comment_ids

postRethreadWikiCommentR :: Text -> Text -> CommentId -> Handler Html
postRethreadWikiCommentR project_handle target comment_id = do
    -- TODO (0): AVOID CYCLES

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
                    Entity new_project_id _ <- getByErr "could not find project" $ UniqueProjectHandle new_project_handle

                    when (new_project_id /= project_id) $ requireModerator "You must be a moderator to rethread." new_project_handle user_id

                    maybe_new_page <- runDB $ getBy $ UniqueWikiTarget new_project_id new_target

                    let new_page = maybe (error "could not find new page") entityVal maybe_new_page

                    return (Nothing, wikiPageDiscussion new_page)

                Nothing -> error "failed to parse URL"

                _ -> error "could not find discussion for that URL"

            when (new_parent_id == commentParent comment && new_discussion_id == commentDiscussion comment) $
                error "trying to move comment to its current location"

            new_parent_depth <- maybe (return $ -1) (fmap commentDepth . runDB . get404) new_parent_id
            old_parent_depth <- maybe (return $ -1) (fmap commentDepth . runDB . get404) $ commentParent comment

            let depth_offset = old_parent_depth - new_parent_depth

            mode <- lookupPostParam "mode"
            let action :: Text = "rethread"
            case mode of
                Just "preview" -> error "no preview for rethreads yet" -- TODO

                Just action' | action' == action -> do
                    now <- liftIO getCurrentTime

                    runDB $ do
                        descendents <- fmap (map (\ (Value x) -> x)) $ select $ from $ \ comment_ancestor -> do
                                where_ $ comment_ancestor ^. CommentAncestorAncestor ==. val comment_id
                                orderBy [ asc $ comment_ancestor ^. CommentAncestorComment ]
                                return $ comment_ancestor ^. CommentAncestorComment

                        let comments = comment_id : descendents

                        rethread_id <- insert $ Rethread now user_id comment_id reason

                        new_comment_ids <- rethreadComments rethread_id depth_offset new_parent_id new_discussion_id comments

                        delete $ from $ \ ancestor -> where_ $ ancestor ^. CommentAncestorComment `in_` valList comments

                        forM_ new_comment_ids $ \ new_comment_id -> do
                            insertSelect $ from $ \ (c `InnerJoin` a) -> do
                                on_ $ c ^. CommentParent ==. just (a ^. CommentAncestorComment)
                                where_ $ c ^. CommentId ==. val new_comment_id
                                return $ CommentAncestor <# val new_comment_id <&> (a ^. CommentAncestorAncestor)

                            [ Value maybe_new_parent_id ] <- select $ from $ \ c -> do
                                where_ $ c ^. CommentId ==. val new_comment_id
                                return (c ^. CommentParent)

                            maybe (return ()) (insert_ . CommentAncestor new_comment_id) maybe_new_parent_id


                        when (new_discussion_id /= commentDiscussion comment) $ update $ \ c -> do
                                where_ $ c ^. CommentId `in_` valList descendents
                                set c [ CommentDiscussion =. val new_discussion_id ]

                    redirect new_parent_url

                m -> error $ "Error: unrecognized mode (" ++ show m ++ ")"
        _ -> error "Error when submitting form."


