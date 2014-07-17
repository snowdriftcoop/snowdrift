module View.Comment
    ( closedForm
    , commentCassius
    , commentEditForm
    , commentEditFormWidget
    , commentNewTopicForm
    , commentReplyForm
    , commentForm
    , commentFormWidget
    , commentTreeWidget
    , commentTreeWidgetNoCSS
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

-- import           Control.Lens  ((%~), _3)
import qualified Data.List                 as L
import qualified Data.Map                  as M
import qualified Data.Text                 as T
import           Data.Tree

disabledCommentForm :: Form Markdown
disabledCommentForm = renderBootstrap3 $ areq snowdriftMarkdownField ("Reply" { fsAttrs = [("disabled",""), ("class","form-control")] }) Nothing

commentForm :: SomeMessage App -> Maybe Markdown -> Form Markdown
commentForm label = renderBootstrap3 . areq' snowdriftMarkdownField label

commentFormWidget :: SomeMessage App -> Maybe Markdown -> Widget
commentFormWidget label content = do
    (comment_form, enctype) <- handlerToWidget $ generateFormPost (commentForm label content)
    [whamlet|
        <div>
            <form method="POST" enctype=#{enctype}>
                ^{comment_form}
                <input type="submit" name="mode" value="preview">
    |]

commentEditForm :: Markdown -> Form Markdown
commentEditForm = commentForm "Edit" . Just

commentEditFormWidget :: Markdown -> Widget
commentEditFormWidget = commentFormWidget "Edit" . Just

commentNewTopicForm :: Form Markdown
commentNewTopicForm = commentForm "New Topic" Nothing

commentReplyForm :: Form Markdown
commentReplyForm = commentForm "Reply" Nothing

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

flagCommentForm :: Form (Maybe [FlagReason], Maybe Markdown)
flagCommentForm = renderBootstrap3 $ (,) <$> flagReasonsForm <*> additionalCommentsForm
  where
    flagReasonsForm :: AForm Handler (Maybe [FlagReason])
    flagReasonsForm = aopt (checkboxesFieldList reasons) "" Nothing
      where
        reasons :: [(Text, FlagReason)]
        reasons = map (descFlagReason &&& id) [minBound..maxBound]

    additionalCommentsForm :: AForm Handler (Maybe Markdown)
    additionalCommentsForm = aopt' snowdriftMarkdownField "Optional: add helpful comments to clarify the issue and/or suggestions for improvement" Nothing

-- | An entire comment tree.
commentTreeWidget :: Widget                -- ^ Form to display under the root comment.
                  -> Tree (Entity Comment) -- ^ Comment tree.
                  -> [CommentClosure]      -- ^ Earlier closures.
                  -> UserMap
                  -> ClosureMap
                  -> TicketMap
                  -> FlagMap
                  -> TagMap
                  -> Text                  -- ^ Project handle.
                  -> Text                  -- ^ Wiki page name.
                  -> Bool                  -- ^ Show actions? (false, for preview)
                  -> Int                   -- ^ Max depth.
                  -> Int                   -- ^ Depth.
                  -> Widget
commentTreeWidget form_under_root_comment
                  comment_tree
                  earlier_closures
                  user_map
                  closure_map
                  ticket_map
                  flag_map
                  tag_map
                  project_handle
                  target
                  show_actions
                  max_depth
                  depth = do
    commentTreeWidgetNoCSS
      form_under_root_comment
      comment_tree
      earlier_closures
      user_map
      closure_map
      ticket_map
      flag_map
      tag_map
      project_handle
      target
      show_actions
      max_depth
      depth
    commentCassius

commentCassius :: Widget
commentCassius = toWidget [cassius|
  .comment
      padding : 0 .8em 0.3em 1em
      margin-top : 1.8em
      border-bottom-left-radius: 1em
      font-size : 15px

  .comment p, .comment ul, .comment ol, .comment h1, .comment h2, .comment h3, .comment h4, .comment h5, .comment h6
      margin : .5em 0

  .comment figure
      text-align : left

  .comment h1
      font-size : large

  .comment h2
      font-size : medium

  .comment blockquote
      font-size : 14px
      margin : 1em

  .comment-head, .comment-action
      margin-right : 1em
      margin-bottom : 0.5em
      display : inline-flex
      padding : 2px 5px

  .top_level
      border-left : solid black 0.2em

  .even_depth
      border-left : solid lightblue 0.2em

  .odd_depth
      border-left : solid lightgrey 0.2em

  .small_avatar
      width : 2.5em
      height : 2.5em
      padding : 0em
      margin-right : 0.3em
      border-radius : 5px

  .closed
      color : goldenrod
      border-left : solid goldenrod 0.25em
      padding-left : 0.5em

  .retracted
      color : darkred
      border-left : solid darkred 0.25em
      padding-left : 0.5em

  .flagged
      border : thin solid darkred
      border-left : solid red
      padding : .5em
      margin : .5em 0
      display : table

  .flag-reasons
      color : darkred
      max-width : 41em

  .flag-reasons, .flag-markdown
      background : whitesmoke
      padding : .5em
      margin : .5em 0
      display : table

  .preview
      visibility : hidden
|]

-- | An entire comment tree, without CSS.
commentTreeWidgetNoCSS :: Widget                -- ^ Form to display under the root comment.
                       -> Tree (Entity Comment) -- ^ Comment tree.
                       -> [CommentClosure]      -- ^ Earlier closures.
                       -> UserMap
                       -> ClosureMap
                       -> TicketMap
                       -> FlagMap
                       -> TagMap
                       -> Text                  -- ^ Project handle.
                       -> Text                  -- ^ Wiki page name.
                       -> Bool                  -- ^ Show actions? (false, for preview)
                       -> Int                   -- ^ Max depth.
                       -> Int                   -- ^ Depth.
                       -> Widget
commentTreeWidgetNoCSS form_under_root_comment
                       (Node root_entity@(Entity root_id root) children)
                       earlier_closures
                       user_map
                       closure_map
                       ticket_map
                       flag_map
                       tag_map
                       project_handle
                       target
                       show_actions
                       max_depth
                       depth = do
    let inner_widget =
            form_under_root_comment <>
            expandCommentOrChildrenWidget
                children
                user_map
                closure_map
                ticket_map
                flag_map
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
        (M.lookup root_id flag_map)
        tag_map
        project_handle
        target
        show_actions
        inner_widget

expandCommentOrChildrenWidget :: [Tree (Entity Comment)]       -- ^ Children comments.
                              -> UserMap
                              -> ClosureMap
                              -> TicketMap
                              -> FlagMap
                              -> TagMap
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
                              flag_map
                              tag_map
                              project_handle
                              target
                              show_actions
                              max_depth
                              depth = do
    let num_children = length children
    if depth > max_depth && num_children > 0
        then expandCommentWidget num_children (max_depth + 2) -- FIXME(mitchell): arbitrary '2' here
        else forM_ children $ \child ->
                 commentTreeWidgetNoCSS
                     mempty
                     child
                     [] -- don't want to show earlier closures on *all* comments, just the first one.
                     user_map
                     closure_map
                     ticket_map
                     flag_map
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
commentWidget :: Entity Comment                       -- ^ Comment.
              -> [CommentClosure]                     -- ^ Earlier closures.
              -> User                                 -- ^ Comment poster.
              -> Maybe CommentClosure                 -- ^ Is this closed?
              -> Maybe (Entity Ticket)                -- ^ Is this a ticket?
              -> Maybe (Maybe Markdown, [FlagReason]) -- ^ Is this comment flagged?
              -> TagMap                               -- ^ Tag map.
              -> Text                                 -- ^ Project handle.
              -> Text                                 -- ^ Wiki page name.
              -> Bool                                 -- ^ Show actions?
              -> Widget                               -- ^ Inner widget (children comments, 'expand' link, reply box, etc)
              -> Widget
commentWidget c@(Entity comment_id comment)
              earlier_closures
              user
              mclosure
              mticket
              mflag
              tag_map
              project_handle
              target
              show_actions
              inner_widget = do
    let user_id       = commentUser comment
        is_unapproved = not . isApproved $ comment
        is_top_level  = isTopLevel  comment
        is_even_depth = isEvenDepth comment
        is_odd_depth  = isOddDepth  comment

    (is_mod, can_establish, can_reply, can_retract, can_close, can_edit, can_delete, can_rethread, can_add_tag, can_flag) <-
        handlerToWidget $ makeViewerPermissions (Entity user_id user) project_handle target c

    tags <- fmap (L.sortBy (compare `on` atName)) . handlerToWidget $ do
        runDB (getCommentTags comment_id) >>=
          annotateCommentTags tag_map project_handle target comment_id . map entityVal

    -- Don't add a comment.cassius file! Instead, modify commentCassius in this file.
    $(widgetFile "comment")

makeViewerPermissions :: Entity User    -- comment poster
                      -> Text
                      -> Text
                      -> Entity Comment
                      -> Handler ( Bool -- is moderator?
                                 , Bool -- can establish?
                                 , Bool -- can reply?
                                 , Bool -- can retract?
                                 , Bool -- can close?
                                 , Bool -- can edit?
                                 , Bool -- can delete?
                                 , Bool -- can rethread?
                                 , Bool -- can add tag?
                                 , Bool -- can flag?
                                 )
makeViewerPermissions (Entity poster_id poster) project_handle target comment_entity@(Entity comment_id comment) = do
    Just current_route <- getCurrentRoute
    let can_reply = not (current_route == ReplyCommentR project_handle target comment_id)

    maybeAuth >>= \case
        Nothing -> return (False, False, can_reply, False, False, False, False, False, False, False)
        Just (Entity viewer_id viewer) -> do
            (is_mod, can_delete) <- runDB $ (,)
                <$> isProjectModerator project_handle viewer_id
                <*> canDeleteComment viewer_id comment_entity

            let can_establish = is_mod && estIsUnestablished (userEstablished poster)
                can_retract   = poster_id == viewer_id
                can_close     = isEstablished viewer
                can_edit      = canEditComment viewer_id comment
                can_rethread  = poster_id == viewer_id || is_mod
                can_add_tag   = isEstablished viewer
                can_flag      = isEstablished viewer && viewer_id /= poster_id

            return (is_mod, can_establish, can_reply, can_retract, can_close, can_edit,
                    can_delete, can_rethread, can_add_tag, can_flag)

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
