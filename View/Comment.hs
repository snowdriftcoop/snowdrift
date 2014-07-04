module View.Comment
    ( closedForm
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

    (is_mod, can_rethread, can_retract, can_close) <- handlerToWidget $ makeViewerPermissions user_id project_handle

    tags <- fmap (L.sortBy (compare `on` atName)) . handlerToWidget $ do
        runDB (getCommentTags comment_id) >>=
          annotateCommentTags tag_map project_handle target comment_id . map entityVal

    $(widgetFile "comment")

-- TODO: Does ViewerPermissions belong elsewhere?

type ViewerPermissions = (Bool, Bool, Bool, Bool)

makeViewerPermissions :: UserId -> Text -> Handler ViewerPermissions
makeViewerPermissions owner_id project_handle = maybeAuth >>= \case
    Nothing -> return (False, False, False, False)
    Just (Entity viewer_id viewer) -> do
        is_mod <- runDB . isProjectModerator project_handle $ viewer_id
        let can_rethread = owner_id == viewer_id || is_mod
            can_retract  = owner_id == viewer_id
            can_close    = isEstablished viewer
        return (is_mod, can_rethread, can_retract, can_close)

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
