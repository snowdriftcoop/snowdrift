module View.Comment
    ( CommentMods(..)
    , closedForm
    , commentWidget
    , commentWidgetMod
    , commentForm
    , commentFormWidget
    , commentTreeWidget
    , commentTreeWithReplyWidget
    , createCommentTagForm
    , disabledCommentForm
    , flagCommentForm
    , newCommentTagForm
    , orderingNewestFirst
    , requiredMarkdownForm
    , rethreadForm
    , retractedForm
    ) where

import Import

import Model.AnnotatedTag
import Model.Comment
import Model.Markdown
import Model.Tag               (TagMap)
import Model.User
import Widgets.Markdown
import Widgets.Tag
import Widgets.Time

import qualified Data.List     as L
import qualified Data.Map      as M
import qualified Data.Text     as T
import           Data.Tree

disabledCommentForm :: Form Markdown
disabledCommentForm = renderBootstrap3 $ areq snowdriftMarkdownField ("Reply" { fsAttrs = [("disabled",""), ("class","form-control")] }) Nothing

commentForm :: Maybe CommentId -> Maybe Markdown -> Form Markdown
commentForm parent content =
    let comment_label = if isJust parent then "Reply" else "New Topic"
     in renderBootstrap3 $ areq' snowdriftMarkdownField comment_label content

commentFormWidget :: Maybe CommentId -> Maybe Markdown -> Widget
commentFormWidget parent content = do
    (comment_form, enctype) <- handlerToWidget $ generateFormPost (commentForm parent content)
    [whamlet|
        <div>
            <form method="POST" enctype=#{enctype}>
                ^{comment_form}
                <input type="submit" name="mode" value="preview">
    |]

rethreadForm :: Form (Text, Text)
rethreadForm = renderBootstrap3 $ (,)
    <$> areq' textField "New Parent Url" Nothing
    <*> areq' textField "Reason" Nothing

createCommentTagForm :: Form Text
createCommentTagForm = renderBootstrap3 $ areq textField "" Nothing

newCommentTagForm :: [Entity Tag] -> [Entity Tag] -> Form (Maybe [TagId], Maybe [TagId])
newCommentTagForm project_tags other_tags = renderBootstrap3 $ (,)
    -- <$> fmap (\(Entity tag_id tag) -> aopt checkBoxField (tag_id) (tagName tag)) (project_tags <> other_tags)
    <$> aopt (tagCloudField $ tags project_tags) "Tags used elsewhere in this project:" Nothing
    <*> aopt (tagCloudField $ tags other_tags) "Tags used in other projects:" Nothing
--    <*> areq hiddenField "" (Just "apply")
    where tags = fmap (\(Entity tag_id tag) -> (tagName tag, tag_id))
          tagCloudField = checkboxesFieldList' $ (\(PersistInt64 a) -> show a) . unKey

closedForm, retractedForm :: Maybe Markdown -> Form Markdown
closedForm    = requiredMarkdownForm "Reason for closing:"
retractedForm = requiredMarkdownForm "Reason for retracting:"

requiredMarkdownForm :: FieldSettings App -> Maybe Markdown -> Form Markdown
requiredMarkdownForm settings = renderBootstrap3 . areq snowdriftMarkdownField settings

flagCommentForm :: Form ([FlagReason], Maybe Markdown)
flagCommentForm = renderBootstrap3 $ (,) <$> flagReasonsForm <*> additionalCommentsForm
  where
    flagReasonsForm :: AForm Handler [FlagReason]
    flagReasonsForm = areq (nonemptyCheckboxesFieldList reasons) "Code of Conduct Violation(s)" Nothing
      where
        nonemptyCheckboxesFieldList :: [(Text, FlagReason)] -> Field Handler [FlagReason]
        nonemptyCheckboxesFieldList = checkBool (not . null) ("Please select one or more violations." :: Text) . checkboxesFieldList

        reasons :: [(Text, FlagReason)]
        reasons = [ ("Personal attack",          FlagPersonalAttack)
                  , ("Unconstructive criticism", FlagUnconstructiveCriticism)
                  , ("Condescension",            FlagCondescension)
                  , ("Defensiveness",            FlagDefensiveness)
                  , ("Spamming",                 FlagSpamming)
                  , ("Privacy violation",        FlagPrivacyViolation)
                  , ("Hate speech",              FlagHateSpeech)
                  ]

    additionalCommentsForm :: AForm Handler (Maybe Markdown)
    additionalCommentsForm = aopt' snowdriftMarkdownField "Optional: add helpful comments to clarify the issue and/or suggestions for improvement" Nothing

-- | An entire comment tree.
commentTreeWidget :: Tree (Entity Comment)         -- ^ Comment tree.
                  -> [CommentClosure]              -- ^ Earlier closures.
                  -> Map UserId User               -- ^ Comment poster.
                  -> Map CommentId CommentClosure  -- ^ Closure map.
                  -> Map CommentId (Entity Ticket) -- ^ Ticket map.
                  -> Map TagId Tag                 -- ^ Tag map.
                  -> Text                          -- ^ Project handle.
                  -> Text                          -- ^ Wiki page name.
                  -> Bool                          -- ^ Show actions? (false, for preview)
                  -> Int                           -- ^ Max depth.
                  -> Int                           -- ^ Depth.
                  -> Widget
commentTreeWidget = commentTreeWithReplyWidget mempty

-- | An entire comment tree, with a reply box underneath the root comment.
commentTreeWithReplyWidget :: Widget                -- ^ Reply form.
                           -> Tree (Entity Comment) -- ^ Comment tree.
                           -> [CommentClosure]      -- ^ Earlier closures.
                           -> UserMap
                           -> ClosureMap
                           -> TicketMap
                           -> TagMap
                           -> Text                  -- ^ Project handle.
                           -> Text                  -- ^ Wiki page name.
                           -> Bool                  -- ^ Show actions? (false, for preview)
                           -> Int                   -- ^ Max depth.
                           -> Int                   -- ^ Depth.
                           -> Widget
commentTreeWithReplyWidget reply_form
                           (Node root_entity@(Entity root_id root) children)
                           earlier_closures
                           user_map
                           closure_map
                           ticket_map
                           tag_map
                           project_handle
                           target
                           show_actions
                           max_depth
                           depth = do
    let inner_widget =
            reply_form <>
            expandCommentOrChildrenWidget
                children
                user_map
                closure_map
                ticket_map
                tag_map
                project_handle
                target
                show_actions
                max_depth
                (depth+1)

    commentWidget
        root_entity
        earlier_closures
        (user_map M.! commentUser root)
        (M.lookup root_id closure_map)
        (M.lookup root_id ticket_map)
        tag_map
        project_handle
        target
        show_actions
        inner_widget

expandCommentOrChildrenWidget :: [Tree (Entity Comment)]       -- ^ Children comments.
                              -> Map UserId User               -- ^ Comment poster.
                              -> Map CommentId CommentClosure  -- ^ Closure map.
                              -> Map CommentId (Entity Ticket) -- ^ Ticket map.
                              -> Map TagId Tag                 -- ^ Tag map.
                              -> Text                          -- ^ Project handle.
                              -> Text                          -- ^ Wiki page name.
                              -> Bool                          -- ^ Show actions? (false, for preview)
                              -> Int                           -- ^ Max depth.
                              -> Int                           -- ^ Depth.
                              -> Widget
expandCommentOrChildrenWidget children
                              user_map
                              closure_map
                              ticket_map
                              tag_map
                              project_handle
                              target
                              show_actions
                              max_depth
                              depth = do
    let num_children = length children
    if depth > max_depth && num_children > 0
        then expandCommentWidget num_children (max_depth + 2) -- FIXME: arbitrary '2' here
        else forM_ children $ \child ->
                 commentTreeWidget
                     child
                     [] -- don't want to show earlier closures on *all* comments, just the first one.
                     user_map
                     closure_map
                     ticket_map
                     tag_map
                     project_handle
                     target
                     show_actions
                     max_depth
                     depth

-- | A "single" comment, which also displays an 'inner widget' inside of it.
-- The reason this can't be made more modular is the HTML for nested comments
-- requires us to render the entire tree (can't close the parent comment's div
-- before the children comments).
commentWidget :: Entity Comment        -- ^ Comment.
              -> [CommentClosure]      -- ^ Earlier closures.
              -> User                  -- ^ Comment poster.
              -> Maybe CommentClosure  -- ^ Is this closed?
              -> Maybe (Entity Ticket) -- ^ Is this a ticket?
              -> Map TagId Tag         -- ^ Tag map.
              -> Text                  -- ^ Project handle.
              -> Text                  -- ^ Wiki page name.
              -> Bool                  -- ^ Show actions?
              -> Widget                -- ^ Inner widget (children comments, 'expand' link, reply box, etc)
              -> Widget
commentWidget (Entity comment_id comment)
              earlier_closures
              user
              mclosure
              mticket
              tag_map
              project_handle
              target
              show_actions
              inner_widget = do
    Just current_route <- getCurrentRoute

    let user_id = commentUser comment
        is_unapproved = not . isApproved $ comment
        is_top_level  = isTopLevel  comment
        is_even_depth = isEvenDepth comment
        is_odd_depth  = isOddDepth  comment
        can_reply     = not (current_route == ReplyCommentR project_handle target comment_id)
        can_establish = estIsUnestablished (userEstablished user)

    (is_mod, can_rethread, can_retract, can_close, can_add_tag) <-
        handlerToWidget $ makeViewerPermissions user_id project_handle

    tags <- fmap (L.sortBy (compare `on` atName)) . handlerToWidget $ do
        runDB (getCommentTags comment_id) >>=
          annotateCommentTags tag_map project_handle target comment_id . map entityVal

    $(widgetFile "comment")

makeViewerPermissions :: UserId -> Text -> Handler (Bool, Bool, Bool, Bool, Bool)
makeViewerPermissions owner_id project_handle = maybeAuth >>= \case
    Nothing -> return (False, False, False, False, False)
    Just (Entity viewer_id viewer) -> do
        is_mod <- runDB . isProjectModerator project_handle $ viewer_id
        let can_rethread = owner_id == viewer_id || is_mod
            can_retract  = owner_id == viewer_id
            can_close    = isEstablished viewer
            can_add_tag  = isEstablished viewer
        return (is_mod, can_rethread, can_retract, can_close, can_add_tag)

-- Order comment trees by newest-first, taking the root and all children of each
-- tree into consideration.
orderingNewestFirst :: Tree (Entity Comment) -> Tree (Entity Comment) -> Ordering
orderingNewestFirst = flip (compare `on` (timestamp . newest))
  where
    newest :: Tree (Entity Comment) -> Entity Comment
    newest = L.maximumBy (compare `on` timestamp) . flatten

    timestamp :: Entity Comment -> UTCTime
    timestamp = commentCreatedTs . entityVal

expandCommentWidget :: Int -> Int -> Widget
expandCommentWidget num_replies new_max_depth = do
    Just cur_route <- getCurrentRoute
    [whamlet|
        <br>
        <br>
        <em>
            <a href="@?{(cur_route, [("maxdepth", T.pack (show new_max_depth))])}">
                #{num_replies} more #{plural num_replies "reply" "replies"}
    |]

-- | Helper method to create a Widget for a comment action (/, /reply, /moderate, etc).
-- Returns a Widget from a Handler (rather than just calling defaultLayout) so that the widget
-- can be put in a preview (for some POST handlers).
commentWidget :: Handler Int    -- ^ Max depth getter.
              -> Bool           -- ^ Show actions?
              -> Widget         -- ^ Widget to display under root comment.
              -> Text           -- ^ Project handle.
              -> Text           -- ^ Target.
              -> CommentId      -- ^ Root comment id.
              -> Handler Widget
commentWidget = commentWidgetMod def

-- | Data type used in commentWidgetMod, containing modifications to comment-action-related
-- data structures.
data CommentMods = CommentMods
    { mod_earlier_closures :: [CommentClosure] -> [CommentClosure]
    , mod_user_map         :: UserMap          -> UserMap
    , mod_closure_map      :: ClosureMap       -> ClosureMap
    , mod_ticket_map       :: TicketMap        -> TicketMap
    , mod_tag_map          :: TagMap           -> TagMap
    }

instance Default CommentMods where
    def = CommentMods id id id id id

-- | Like @commentWidget@, but includes modifications to the datastructures grabbed from
-- the database. This is used for showing previews of comment trees, where changes are not
-- saved yet.
commentWidgetMod :: CommentMods    -- ^ Comment structure modifications.
                 -> Handler Int    -- ^ Max depth getter.
                 -> Bool           -- ^ Is preview_
                 -> Widget         -- ^ Widget to display under root comment.
                 -> Text           -- ^ Project handle.
                 -> Text           -- ^ Target.
                 -> CommentId      -- ^ Root comment id.
                 -> Handler Widget
commentWidgetMod CommentMods{..} get_max_depth show_actions form project_handle target comment_id = do
    redirectIfRethreaded project_handle comment_id
    (Entity project_id _, _, root) <- checkCommentPage project_handle target comment_id

    mviewer_id <- maybeAuthId
    (rest, user_map, earlier_closures, closure_map, ticket_map, tag_map) <- runDB $ do
        rest <- getCommentDescendants mviewer_id project_id comment_id

        let all_comments    = (Entity comment_id root):rest
            all_comment_ids = map entityKey all_comments

        earlier_closures <- getAncestorClosures comment_id
        user_map         <- entitiesMap <$> getUsersIn (S.toList $ getCommentsUsers all_comments)
        closure_map      <- makeClosureMap all_comment_ids
        ticket_map       <- makeTicketMap  all_comment_ids
        tag_map          <- entitiesMap <$> getAllTags

        return (rest, user_map, earlier_closures, closure_map, ticket_map, tag_map)

    user_map_with_viewer <- (maybe id (\(Entity viewer_id viewer) -> M.insert viewer_id viewer))
        <$> maybeAuth
        <*> pure user_map

    max_depth <- get_max_depth
    return $
        commentTreeWithReplyWidget
            form
            (sortTreeBy orderingNewestFirst $ buildCommentTree (Entity comment_id root, rest))
            (mod_earlier_closures earlier_closures)
            (mod_user_map user_map_with_viewer)
            (mod_closure_map closure_map)
            (mod_ticket_map ticket_map)
            (mod_tag_map tag_map)
            project_handle
            target
            show_actions
            max_depth
            0

