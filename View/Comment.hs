module View.Comment where

import Import

import qualified Data.Foldable as F
import qualified Data.List     as L
import qualified Data.Map      as M
import           Data.Tree

import Model.AnnotatedTag
import Model.CollapseState
import Model.Comment           (getCommentTags)
import Model.Markdown
import Model.User
import Widgets.Markdown
import Widgets.Tag
import Widgets.Time


countReplies :: [Tree a] -> Int
countReplies = sum . map (F.sum . fmap (const 1))

disabledCommentForm :: Form Markdown
disabledCommentForm = renderBootstrap3 $ areq snowdriftMarkdownField ("Reply" { fsAttrs = [("disabled",""), ("class","form-control")] }) Nothing

commentForm :: Maybe CommentId -> Maybe Markdown -> Form Markdown
commentForm parent content =
    let comment_label = if isJust parent then "Reply" else "New Topic"
     in renderBootstrap3 $ areq' snowdriftMarkdownField comment_label content

-- | Render a single comment tree. The children comments (a comment forest) will be sorted by
-- a call to commentForestWidget, which is mutually recursive.
commentTreeWidget :: Tree (Entity Comment)         -- ^ Comment tree.
                  -> [CommentClosure]              -- ^ Earlier closures.
                  -> Map UserId User               -- ^ Comment poster.
                  -> Map CommentId CommentClosure  -- ^ Closure map.
                  -> Map CommentId (Entity Ticket) -- ^ Ticket map.
                  -> Map TagId Tag                 -- ^ Tag map.
                  -> Text                          -- ^ Project handle.
                  -> Text                          -- ^ Wiki page name.
                  -> Int                           -- ^ Max depth.
                  -> Int                           -- ^ Depth.
                  -> Bool                          -- ^ Show actions? (false, for preview)
                  -> Maybe Widget                  -- ^ Comment form.
                  -> Widget
commentTreeWidget (Node (Entity comment_id comment) children)
                  earlier_closures
                  user_map
                  closure_map
                  ticket_map
                  tag_map
                  project_handle
                  target
                  max_depth
                  depth
                  show_actions
                  mcomment_form = do
    Just current_route <- getCurrentRoute

    let user_id     = commentUser comment
        user        = user_map M.! user_id
        mclosure    = M.lookup comment_id closure_map
        mticket     = M.lookup comment_id ticket_map

    (is_mod, can_rethread, can_retract, can_close) <- handlerToWidget $ makeViewerPermissions user_id project_handle

    let is_unapproved = not . isJust $ commentModeratedTs comment
        is_top_level  = commentDepth comment == 0
        is_even_depth = not is_top_level && commentDepth comment `mod` 2 == 1
        is_odd_depth  = not is_top_level && not is_even_depth

    tags <- fmap (L.sortBy (compare `on` atName)) . handlerToWidget $ do
        runDB (getCommentTags comment_id) >>=
          annotateCommentTags tag_map project_handle target comment_id . map entityVal

    collapse_state <- return FullyVisible -- maybe (return FullyVisible) (handlerToWidget . collapseState now) maybe_closure

    case (depth == 0, collapse_state) of
        (True, _) -> $(widgetFile "comment_body")
        (_, FullyVisible) -> $(widgetFile "comment_body")

        (_, Collapsed) -> -- TODO: prettify, unify with messages in comment_body
            [whamlet|
                $case maybe Closed commentClosureType mclosure
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

-- | Render a forest of comments.
commentForestWidget :: Forest (Entity Comment)       -- ^ Comment forest.
                    -> [CommentClosure]              -- ^ Earlier closures.
                    -> Map UserId User               -- ^ User map.
                    -> Map CommentId CommentClosure  -- ^ Closure map.
                    -> Map CommentId (Entity Ticket) -- ^ Ticket (if it is one).
                    -> Map TagId Tag                 -- ^ Tag map.
                    -> Text                          -- ^ Project handle.
                    -> Text                          -- ^ Wiki page name.
                    -> Int                           -- ^ Max depth, per tree.
                    -> Int                           -- ^ Depth, per tree.
                    -> Bool                          -- ^ Show actions? (false, for preview)
                    -> Maybe Widget                  -- ^ Comment form.
                    -> Widget
commentForestWidget comment_forest
                    earlier_closures
                    user_map
                    closure_map
                    ticket_map
                    tag_map
                    project_handle
                    target
                    max_depth
                    depth
                    show_actions
                    mcomment_form = do
    forM_ comment_forest $ \comment_tree ->
        commentTreeWidget
            comment_tree
            earlier_closures
            user_map
            closure_map
            ticket_map
            tag_map
            project_handle
            target
            max_depth
            depth
            show_actions
            mcomment_form

-- | discussCommentTreeWidget is for permalink views of particular comments
discussCommentTreeWidget :: Tree (Entity Comment)
                         -> [CommentClosure]
                         -> Map UserId User
                         -> Map CommentId CommentClosure
                         -> Map CommentId (Entity Ticket)
                         -> Map TagId Tag
                         -> Text
                         -> Text
                         -> Bool
                         -> Maybe Widget
                         -> Widget
discussCommentTreeWidget comment_tree
                         earlier_closures
                         user_map
                         closure_map
                         ticket_map
                         tag_map
                         project_handle
                         target
                         show_actions
                         mcomment_form = do
    let comment = commentTreeWidget
                      comment_tree
                      earlier_closures
                      user_map
                      closure_map
                      ticket_map
                      tag_map
                      project_handle
                      target
                      11
                      0
                      show_actions
                      mcomment_form
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

