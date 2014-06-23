module Handler.Discussion where

import Import

import qualified Control.Monad.State        as St
import           Data.Foldable              (Foldable)
import qualified Data.Foldable              as F
import qualified Data.Map                   as M
import qualified Data.Set                   as S
import qualified Data.Text                  as T
import           Network.HTTP.Types.Status
import           Yesod.Default.Config
import           Yesod.Markdown

import qualified Data.Tree.Extra            as Tree
import           Data.Tree.Extra            (sortForestBy, sortTreeBy)
import           Model.ClosureType
import           Model.Comment              -- (buildCommentTree, ...) TODO
import           Model.Project              (getProjectPages)
import           Model.Role
import           Model.Tag                  (getAllTags)
import           Model.User
import           Model.ViewTime             (getCommentViewTimes)
import           Model.ViewType
import           Model.WikiPage
import           Widgets.Markdown
import           Widgets.Preview
import           View.Comment               ( commentForm, commentForestWidget, commentTreeWidget
                                            , discussCommentTreeWidget, orderingNewestFirst
                                            )

checkCommentPage :: CommentId -> WikiPageId -> Handler ()
checkCommentPage comment_id page_id = do
    comment_page_id <- runDB $ getCommentPageId comment_id
    when (comment_page_id /= page_id) $ error "comment does not match page"

requireModerator :: Text -> Text -> UserId -> Handler ()
requireModerator message project_handle user_id = do
    [ Value c :: Value Int ] <- runDB $ select $ from $ \ (pur `InnerJoin` project) -> do
        on_ $ pur ^. ProjectUserRoleProject ==. project ^. ProjectId
        where_ $ pur ^. ProjectUserRoleUser ==. val user_id
                &&. pur ^. ProjectUserRoleRole ==. val Moderator
                &&. project ^. ProjectHandle ==. val project_handle

        return $ count $ pur ^. ProjectUserRoleId

    when (c < 1) $ permissionDenied message

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
    unless (isJust $ userEstablishedTs user) $
        permissionDenied "You must be an established user to close a conversation."

    return (project_id, comment)


getRetractWikiCommentR :: Text -> Text -> CommentId -> Handler Html
getRetractWikiCommentR = getCloseWikiComment Retracted

getCloseWikiCommentR :: Text -> Text -> CommentId -> Handler Html
getCloseWikiCommentR = getCloseWikiComment Closed

getCloseWikiComment :: ClosureType -> Text -> Text -> CommentId -> Handler Html
getCloseWikiComment closure_type project_handle target comment_id = do
    Entity user_id user <- requireAuth

    (_, comment) <- case closure_type of
        Retracted -> checkRetractComment user_id project_handle target comment_id
        Closed -> checkCloseComment user project_handle target comment_id

    let poster_id = commentUser comment
    (poster, earlier_closures, ticket_map, tag_map) <- runDB $ (,,,)
        <$> get404 poster_id
        <*> getAncestorClosures comment_id
        <*> makeTicketMap [comment_id]
        <*> (entitiesMap <$> getTags comment_id)

    let rendered_comment = discussCommentTreeWidget
                               (Tree.singleton (Entity comment_id comment))
                               earlier_closures
                               (M.fromList [(user_id, user), (poster_id, poster)])
                               mempty -- closure map
                               ticket_map
                               tag_map
                               project_handle
                               target
                               False
                               Nothing

    (closure_form, _) <- generateFormPost $ closureForm closure_type Nothing
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
    ((result, _), _) <- runFormPost $ closureForm closure_type Nothing

    case result of
        FormSuccess reason -> do
            Entity user_id user <- requireAuth

            (_, comment) <- case closure_type of
                Retracted -> checkRetractComment user_id project_handle target comment_id
                Closed    -> checkCloseComment   user    project_handle target comment_id

            earlier_closures <- runDB $ getAncestorClosures comment_id

            let action :: Text = case closure_type of
                    Closed -> "close"
                    Retracted -> "retract"

            mode <- lookupPostParam "mode"
            case mode of
                Just "preview" -> do
                    (form, _) <- generateFormPost $ closureForm closure_type (Just reason)

                    let poster_id = commentUser comment

                    (poster, ticket_map, tag_map) <- runDB $ (,,)
                        <$> get404 poster_id
                        <*> makeTicketMap [comment_id]
                        <*> (entitiesMap <$> getTags comment_id)
                    closure_map <- M.singleton comment_id <$> newCommentClosure user_id closure_type reason comment_id

                    defaultLayout $ renderPreview form action $
                        discussCommentTreeWidget
                            (Tree.singleton (Entity comment_id comment))
                            earlier_closures
                            (M.fromList [(user_id, user), (poster_id, poster)])
                            closure_map
                            ticket_map
                            tag_map
                            project_handle
                            target
                            False   -- show actions?
                            Nothing -- comment form

                Just a | a == action -> do
                    now <- liftIO getCurrentTime
                    runDB $ insert_ $ CommentClosure now user_id closure_type reason comment_id

                    redirect $ DiscussCommentR project_handle target comment_id

                m -> error $ "Error: unrecognized mode (" ++ show m ++ ")"
        _ -> error "Error when submitting form."

-- | getDiscussWikiR generates the associated discussion page for each wiki page
getDiscussWikiR :: Text -> Text -> Handler Html
getDiscussWikiR project_handle target = lookupGetParam "state" >>= \case
    Just "closed" -> getDiscussWikiR' project_handle target getClosedRootComments
    _             -> getDiscussWikiR' project_handle target getOpenRootComments

getDiscussWikiR' :: Text                                                   -- ^ Project handle.
                 -> Text                                                   -- ^ Wiki page name.
                 -> (Bool -> DiscussionId -> YesodDB App [Entity Comment]) -- ^ Root comment getter.
                 -> Handler Html
getDiscussWikiR' project_handle target get_root_comments = do
    muser <- maybeAuth
    (Entity _ project, Entity _ page) <- getPageInfo project_handle target

    is_moderator <- isCurUserProjectModerator project_handle

    (roots, replies, user_map, closure_map, ticket_map, tag_map) <- runDB $ do
        roots           <- get_root_comments is_moderator (wikiPageDiscussion page)
        replies         <- getRepliesComments is_moderator (wikiPageDiscussion page)
        user_map        <- entitiesMap <$> getUsersIn (S.toList $ get_user_ids roots <> get_user_ids replies)
        let comment_ids  = map entityKey (roots ++ replies)
        closure_map     <- makeClosureMap comment_ids
        ticket_map      <- makeTicketMap comment_ids
        tag_map         <- entitiesMap <$> getAllTags
        return (roots, replies, user_map, closure_map, ticket_map, tag_map)

    let comments = commentForestWidget
                       (sortForestBy orderingNewestFirst (buildCommentForest roots replies))
                       []             -- earlier closures
                       user_map
                       closure_map
                       ticket_map
                       tag_map
                       project_handle
                       target
                       8              -- max depth
                       0              -- depth
                       True           -- show actions?
                       Nothing        -- comment form

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
        FormSuccess text -> do
            mode <- lookupPostParam "mode"
            processWikiComment mode (Just comment_id) text project_entity page
        FormMissing      -> error "Form missing."
        FormFailure msgs -> error $ "Error submitting form: " ++ T.unpack (T.intercalate "\n" msgs)

getDiscussCommentR' :: Bool -> Text -> Text -> CommentId -> Handler Html
getDiscussCommentR' show_reply project_handle target comment_id = do
    runDB (getCommentRethread comment_id) >>= \case
        Nothing -> return ()
        Just destination_comment_id -> do
            -- TODO: any way to statically make sure we've covered all discussion types?
            page_target <- wikiPageTarget <$> runDB (getCommentPage destination_comment_id)
            redirectWith movedPermanently301
                         (let route = if show_reply then ReplyCommentR else DiscussCommentR
                          in route project_handle page_target destination_comment_id)

    (_, Entity page_id _) <- getPageInfo project_handle target

    moderator <- isCurUserProjectModerator project_handle

    (root, rest, user_map, earlier_closures, closure_map, ticket_map, tag_map) <- runDB $ do
        root <- get404 comment_id
        root_wiki_page_id <- getCommentPageId comment_id

        when (root_wiki_page_id /= page_id) $
            error "Selected comment does not match selected page"

        descendants <- getCommentDescendants comment_id

        -- TODO: move to Model/Comment?
        rest <-
            select $
                from $ \c -> do
                where_ (c ^. CommentId `in_` valList descendants &&.
                        if moderator
                            then val True
                            else not_ . isNothing $ c ^. CommentModeratedTs)
                orderBy [asc (c ^. CommentParent), asc (c ^. CommentCreatedTs)]
                return c

        let all_comments    = (Entity comment_id root):rest
            all_comment_ids = map entityKey all_comments
            user_ids = get_user_ids all_comments

        earlier_closures <- getAncestorClosures comment_id
        user_map         <- entitiesMap <$> getUsersIn (S.toList user_ids)
        closure_map      <- makeClosureMap all_comment_ids
        ticket_map       <- makeTicketMap all_comment_ids
        tag_map          <- entitiesMap <$> runDB getAllTags

        return (root, rest, user_map, earlier_closures, closure_map, ticket_map, tag_map)

    comment_form <-
        if show_reply
        then Just . fst <$> generateFormPost (commentForm (Just comment_id) Nothing)
        else return Nothing


    defaultLayout $ discussCommentTreeWidget
                        (sortTreeBy orderingNewestFirst $ buildCommentTree (Entity comment_id root, rest))
                        earlier_closures
                        user_map
                        closure_map
                        ticket_map
                        tag_map
                        project_handle
                        target
                        True -- show actions?
                        comment_form

processWikiComment :: Maybe Text -> Maybe CommentId -> Markdown -> Entity Project -> WikiPage -> Handler Html
processWikiComment mode =
    case mode of
        Just "preview" -> processWikiCommentPreview
        Just "post"    -> processWikiCommentPost
        _              -> error $ "Error: unrecognized mode (" ++ show mode ++ ")"

processWikiCommentPreview :: Maybe CommentId -> Markdown -> Entity Project -> WikiPage -> Handler Html
processWikiCommentPreview maybe_parent_id text (Entity _ project) page = do
    Entity user_id user <- requireAuth

    (earlier_closures, tag_map) <- runDB $ (,)
        <$> maybe (return []) getAncestorClosures' maybe_parent_id
        <*> (entitiesMap <$> getAllTags)

    depth <- depthFromMaybeParentId maybe_parent_id
    now <- liftIO getCurrentTime
    let comment =
          Entity (Key $ PersistInt64 0) $
            Comment now Nothing Nothing Nothing (wikiPageDiscussion page) maybe_parent_id user_id text depth

        rendered_comment = discussCommentTreeWidget
                               (Tree.singleton comment)
                               earlier_closures
                               (M.singleton user_id user)
                               mempty
                               mempty -- TODO: is this right?
                               tag_map
                               (projectHandle project)
                               (wikiPageTarget page)
                               False   -- show actions?
                               Nothing -- comment form

    (form, _) <- generateFormPost $ commentForm maybe_parent_id (Just text)
    defaultLayout $ renderPreview form "post" rendered_comment

processWikiCommentPost :: Maybe CommentId -> Markdown -> Entity Project -> WikiPage -> Handler Html
processWikiCommentPost maybe_parent_id text (Entity _ project) page = do
    Entity user_id user <- requireAuth
    now <- liftIO getCurrentTime
    depth <- depthFromMaybeParentId maybe_parent_id

    let is_established = isEstablished user
    maybe_parent_id' <- runDB $ do
        maybe_parent_id' <- maybe (return Nothing) (fmap Just . getCommentDestination) maybe_parent_id

        comment_id <- insert $ Comment now
                                       (if is_established then Just now else Nothing)
                                       (if is_established then Just user_id else Nothing)
                                       Nothing
                                       (wikiPageDiscussion page)
                                       maybe_parent_id'
                                       user_id
                                       text
                                       depth

        let content = T.lines $ (\ (Markdown str) -> str) text
            tickets = map T.strip $ mapMaybe (T.stripPrefix "ticket:") content
            tags    = map T.strip $ mconcat $ map (T.splitOn ",") $ mapMaybe (T.stripPrefix "tags:") content

        forM_ tickets $ \ ticket -> insert_ $ Ticket now now ticket comment_id
        forM_ tags $ \ tag -> do
            tag_id <- fmap (either entityKey id) $ insertBy $ Tag tag
            insert_ $ CommentTag comment_id tag_id user_id 1

        ancestor_ids <- maybe (return [])
                              (\parent_id -> (parent_id :) <$> getCommentAncestors parent_id)
                              maybe_parent_id

        forM_ ancestor_ids (insert_ . CommentAncestor comment_id)

        update $ \ticket -> do
            set ticket [ TicketUpdatedTs =. val now ]
            where_ $ ticket ^. TicketComment `in_` subGetCommentAncestors comment_id

        return maybe_parent_id'

    addAlert "success" $ if is_established then "comment posted" else "comment submitted for moderation"
    redirect $ maybe (DiscussWikiR (projectHandle project) (wikiPageTarget page)) (DiscussCommentR (projectHandle project) (wikiPageTarget page)) maybe_parent_id'

-- Get the depth of a comment, given (maybe) its parent's CommentId.
depthFromMaybeParentId :: Maybe CommentId -> Handler Int
depthFromMaybeParentId = maybe (return 0) (fmap (+1) . runDB . getCommentDepth)

postDiscussWikiR :: Text -> Text -> Handler Html
postDiscussWikiR project_handle target = do
    (project_entity, Entity _ page) <- getPageInfo project_handle target

    ((result, _), _) <- runFormPost $ commentForm Nothing Nothing

    case result of
        FormSuccess text -> do
            mode <- lookupPostParam "mode"
            processWikiComment mode Nothing text project_entity page
        FormMissing      -> error "Form missing."
        FormFailure msgs -> error $ "Error submitting form: " ++ T.unpack (T.intercalate "\n" msgs)


getNewDiscussWikiR :: Text -> Text -> Handler Html
getNewDiscussWikiR project_handle target = do
    void requireAuth
    let action = DiscussWikiR project_handle target

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
                comments_ts <- runDB (getCommentViewTimes viewer_id project_id) >>= \case
                    []         -> return (userReadComments viewer)
                    viewtime:_ -> return (viewTimeTime viewtime)
                redirectParams (WikiNewCommentsR project_handle) $ (T.pack "since", T.pack $ show comments_ts) : reqGetParams req
            Just since -> return (read . T.unpack $ since)

    maybe_from <- fmap (Key . PersistInt64 . read . T.unpack) <$> lookupGetParam "from"

    now <- liftIO getCurrentTime

    tag_map <- entitiesMap <$> runDB getAllTags

    (new_comments, old_comments, users, closure_map, ticket_map) <- runDB $ do
        unfiltered_pages <- getProjectPages project_id

        let pages_map = entitiesMap {- TODO filter ((userRole viewer >=) . wikiPageCanVisitMeta . entityVal) -} unfiltered_pages
            apply_offset comment = maybe id (\from_comment rest -> comment ^. CommentId <=. val from_comment &&. rest) maybe_from

        new_comments :: [Entity Comment] <-
            select $
                from $ \(comment `InnerJoin` wiki_page) -> do
                on_ (comment ^. CommentDiscussion ==. wiki_page ^. WikiPageDiscussion)
                where_ (apply_offset comment (wiki_page ^. WikiPageId `in_` valList (M.keys pages_map)) &&.
                        comment ^. CommentCreatedTs >=. val since)
                orderBy [ desc (comment ^. CommentId) ]
                limit 51
                return comment

        old_comments :: [Entity Comment] <-
            select $
                from $ \(comment `InnerJoin` wiki_page) -> do
                on_ (comment ^. CommentDiscussion ==. wiki_page ^. WikiPageDiscussion)
                where_ (apply_offset comment (wiki_page ^. WikiPageId `in_` valList (M.keys pages_map)) &&.
                        comment ^. CommentCreatedTs <. val since)
                orderBy [ desc (comment ^. CommentId) ]
                limit $ fromIntegral $ 51 - length new_comments
                return comment

        users <- entitiesMap <$> getUsersIn (S.toList . S.fromList $ map (commentUser . entityVal) (new_comments <> old_comments))

        let comment_ids = map entityKey (new_comments <> old_comments)
        closure_map <- makeClosureMap comment_ids
        ticket_map  <- makeTicketMap  comment_ids

        return (new_comments, old_comments, users, closure_map, ticket_map)

    let new_comments' = take 50 new_comments
        old_comments' = take (50 - length new_comments') old_comments
        PersistInt64 to = unKey $ minimum (map entityKey (new_comments' <> old_comments') )
        render_comments comments =
            if null comments
                then [whamlet||]
                else forM_ comments $ \ (Entity comment_id comment) -> do
                    (earlier_closures, target) <- handlerToWidget . runDB $ (,)
                        <$> getAncestorClosures comment_id
                        <*> (wikiPageTarget <$> getCommentPage comment_id)

                    let rendered_comment =
                            commentTreeWidget
                                (Tree.singleton (Entity comment_id comment))
                                earlier_closures
                                users
                                closure_map
                                ticket_map
                                tag_map
                                project_handle
                                target
                                0    -- max_depth is irrelevant for the new-comments listing 0
                                0
                                True -- show actions?
                                Nothing

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
getRethreadWikiCommentR _ _ _ = do
    (form, _) <- generateFormPost rethreadForm
    defaultLayout $(widgetFile "rethread")

rethreadComments :: RethreadId -> Int -> Maybe CommentId -> DiscussionId -> [CommentId] -> YesodDB App [CommentId]
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

get_user_ids :: Foldable f => f (Entity Comment) -> Set UserId
get_user_ids = F.foldMap (S.singleton . commentUser . entityVal)
