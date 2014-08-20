module View.Comment
    ( approveCommentFormWidget
    , closeCommentForm
    , closeCommentFormWidget
    , commentForm
    , commentFormWidget
    , commentNewTopicForm
    , commentNewTopicFormWidget
    , commentReplyForm
    , commentReplyFormWidget
    , commentTreeWidget
    , createCommentTagForm
    , deleteCommentFormWidget
    , disabledCommentForm
    , editCommentForm
    , editCommentFormWidget
    , flagCommentForm
    , flagCommentFormWidget
    , newCommentTagForm
    , orderingNewestFirst
    , rethreadCommentForm
    , rethreadCommentFormWidget
    , retractCommentForm
    , retractCommentFormWidget
    ) where

import Import

import Model.Comment
import Model.Comment.ActionPermissions
import Model.Comment.Routes
import Model.Tag
import Model.User
import View.User
import Widgets.Markdown
import Widgets.Tag
import Widgets.Time

import qualified Data.List as L
import qualified Data.Map  as M
import qualified Data.Text as T
import           Data.Tree

disabledCommentForm :: Form Markdown
disabledCommentForm = renderBootstrap3 $ areq snowdriftMarkdownField ("Reply" { fsAttrs = [("disabled",""), ("class","form-control")] }) Nothing

commentForm :: SomeMessage App -> Maybe Markdown -> Form Markdown
commentForm label = renderBootstrap3 . areq' snowdriftMarkdownField label

commentFormWidget :: SomeMessage App -> Maybe Markdown -> Widget
commentFormWidget label = commentFormWidget' . commentForm label

commentFormWidget' :: Form Markdown -> Widget
commentFormWidget' form = do
    (widget, enctype) <- handlerToWidget $ generateFormPost form
    [whamlet|
        <div>
            <form method="POST" enctype=#{enctype}>
                ^{widget}
                <button type="submit" name="mode" value="preview">preview
    |]

closeCommentForm    :: Maybe Markdown -> Form Markdown
commentNewTopicForm ::                   Form Markdown
commentReplyForm    ::                   Form Markdown
editCommentForm     :: Markdown       -> Form Markdown
retractCommentForm  :: Maybe Markdown -> Form Markdown

closeCommentForm    = commentForm "Reason for closing:"
commentNewTopicForm = commentForm "New Topic" Nothing
commentReplyForm    = commentForm "Reply"     Nothing
editCommentForm     = commentForm "Edit"      . Just
retractCommentForm  = commentForm "Reason for retracting:"

closeCommentFormWidget    :: Maybe Markdown -> Widget
commentNewTopicFormWidget ::                   Widget
commentReplyFormWidget    ::                   Widget
editCommentFormWidget     :: Markdown       -> Widget
retractCommentFormWidget  :: Maybe Markdown -> Widget

closeCommentFormWidget    = commentFormWidget' . closeCommentForm
commentNewTopicFormWidget = commentFormWidget' commentNewTopicForm
commentReplyFormWidget    = commentFormWidget' commentReplyForm
editCommentFormWidget     = commentFormWidget' . editCommentForm
retractCommentFormWidget  = commentFormWidget' . retractCommentForm

approveCommentFormWidget :: Widget
approveCommentFormWidget =
    [whamlet|
        <form method="POST">
            <button type="submit" name="mode" value="post">approve post
    |]

rethreadCommentForm :: Form (Text, Text)
rethreadCommentForm = renderBootstrap3 $ (,)
    <$> areq' textField "New Parent Url" Nothing
    <*> areq' textField "Reason" Nothing

rethreadCommentFormWidget :: Widget
rethreadCommentFormWidget = do
    (form, enctype) <- handlerToWidget (generateFormPost rethreadCommentForm)
    [whamlet|
        <form method=post enctype=#{enctype}>
            ^{form}
            <button type="submit" name="mode" value="post">rethread
    |]

createCommentTagForm :: Form Text
createCommentTagForm = renderBootstrap3 $ areq' textField "" Nothing

newCommentTagForm :: [Entity Tag] -> [Entity Tag] -> Form (Maybe [TagId], Maybe [TagId])
newCommentTagForm project_tags other_tags = renderBootstrap3 $ (,)
    -- <$> fmap (\(Entity tag_id tag) -> aopt checkBoxField (tag_id) (tagName tag)) (project_tags <> other_tags)
    <$> aopt (tagCloudField $ tags project_tags) "Tags used elsewhere in this project:" Nothing
    <*> aopt (tagCloudField $ tags other_tags) "Tags used in other projects:" Nothing
--    <*> areq hiddenField "" (Just "apply")
    where tags = fmap (\(Entity tag_id tag) -> (tagName tag, tag_id))
          tagCloudField = checkboxesFieldList' $ (\(PersistInt64 a) -> show a) . unKey

flagCommentForm :: Maybe (Maybe [FlagReason]) -> Maybe (Maybe Markdown) -> Form (Maybe [FlagReason], Maybe Markdown)
flagCommentForm def_reasons def_message = renderBootstrap3 $ (,) <$> flagReasonsForm <*> additionalCommentsForm
  where
    flagReasonsForm :: AForm Handler (Maybe [FlagReason])
    flagReasonsForm = aopt (checkboxesFieldList reasons) "" def_reasons
      where
        reasons :: [(Text, FlagReason)]
        reasons = map (descFlagReason &&& id) [minBound..maxBound]

    additionalCommentsForm :: AForm Handler (Maybe Markdown)
    additionalCommentsForm = aopt' snowdriftMarkdownField "Optional: add helpful comments to clarify the issue and/or suggestions for improvement" def_message

flagCommentFormWidget :: Maybe (Maybe [FlagReason]) -> Maybe (Maybe Markdown) -> Widget
flagCommentFormWidget def_reasons def_message = do
    (form, enctype) <- handlerToWidget (generateFormPost (flagCommentForm def_reasons def_message))
    [whamlet|
        <form method="POST" enctype=#{enctype}>
            <h4>Code of Conduct Violation(s):
            ^{form}
            <button type="submit" name="mode" value="preview">preview flag message
    |]

deleteCommentFormWidget :: Widget
deleteCommentFormWidget =
    [whamlet|
        <div>
            <form method=POST>
                <button type="submit" name="mode" value="post">delete
                <button type="submit" name="mode" value="cancel">cancel
    |]

-- | Order comment trees by newest-first, taking the root and all children of each
-- tree into consideration (essentially compares each tree's newest comment,
-- no matter how deeply nested).
orderingNewestFirst :: Tree (Entity Comment) -> Tree (Entity Comment) -> Ordering
orderingNewestFirst = flip (compare `on` (timestamp . newest))
  where
    newest :: Tree (Entity Comment) -> Entity Comment
    newest = L.maximumBy (compare `on` timestamp) . flatten

    timestamp :: Entity Comment -> UTCTime
    timestamp = commentCreatedTs . entityVal

expandCommentWidget :: Int -> MaxDepth -> Widget
expandCommentWidget num_replies new_max_depth = do
    Just cur_route <- getCurrentRoute
    let new_route = case new_max_depth of
                        NoMaxDepth -> (cur_route, [])
                        MaxDepth n -> (cur_route, [("maxdepth", T.pack (show n))])
    [whamlet|
        <br>
        <br>
        <em>
            <a href="@?{new_route}">
                #{num_replies} more #{plural num_replies "reply" "replies"}
    |]

-- | An entire comment tree.
commentTreeWidget :: Widget                       -- ^ Form to display under the root comment.
                  -> Tree (Entity Comment)        -- ^ Comment tree.
                  -> Maybe UserId                 -- ^ Viewer.
                  -> CommentRoutes                -- ^ Comment routes.
                  -> MakeCommentActionPermissions -- ^ Action permissions maker.
                  -> [CommentClosure]             -- ^ Earlier closures.
                  -> UserMap
                  -> ClosureMap
                  -> TicketMap
                  -> FlagMap
                  -> Bool                         -- ^ Is preview?
                  -> MaxDepth                     -- ^ Max depth.
                  -> Int                          -- ^ Depth.
                  -> Widget
commentTreeWidget form_under_root_comment
                  (Node root_entity@(Entity root_id root) children)
                  mviewer_id
                  comment_routes
                  make_action_permissions
                  earlier_closures
                  user_map
                  closure_map
                  ticket_map
                  flag_map
                  is_preview
                  max_depth
                  depth = do
    let num_children = length children
        inner_widget =
            form_under_root_comment <>
            if MaxDepth depth > max_depth && num_children > 0
                then expandCommentWidget num_children (addMaxDepth max_depth 2) -- FIXME(mitchell): arbitrary '2' here
                else forM_ children $ \child ->
                         commentTreeWidget
                           mempty
                           child
                           mviewer_id
                           comment_routes
                           make_action_permissions
                           [] -- don't want to show earlier closures on *all* comments, just the first one.
                           user_map
                           closure_map
                           ticket_map
                           flag_map
                           is_preview
                           max_depth
                           (depth+1)

    action_permissions <- handlerToWidget (make_action_permissions root_entity)
    commentWidget
        root_entity
        mviewer_id
        comment_routes
        action_permissions
        earlier_closures
        (user_map M.! commentUser root)
        (M.lookup root_id closure_map)
        (M.lookup root_id ticket_map)
        (M.lookup root_id flag_map)
        is_preview
        inner_widget

-- | A "single" comment, which also displays an 'inner widget' inside of it.
-- The reason this can't be made more modular is the HTML for nested comments
-- requires us to render the entire tree (can't close the parent comment's div
-- before the children comments).
commentWidget :: Entity Comment                       -- ^ Comment.
              -> Maybe UserId                         -- ^ Viewer.
              -> CommentRoutes                        -- ^ Comment routes.
              -> CommentActionPermissions             -- ^ Permissions for comment actions.
              -> [CommentClosure]                     -- ^ Earlier closures.
              -> User                                 -- ^ Comment poster.
              -> Maybe CommentClosure                 -- ^ Is this closed?
              -> Maybe (Entity Ticket)                -- ^ Is this a ticket?
              -> Maybe (Maybe Markdown, [FlagReason]) -- ^ Is this comment flagged?
              -> Bool                                 -- ^ Is this a preview?
              -> Widget                               -- ^ Inner widget (children comments, 'expand' link, reply box, etc)
              -> Widget
commentWidget (Entity comment_id comment)
              mviewer_id
              CommentRoutes{..}
              CommentActionPermissions{..}
              earlier_closures
              user
              mclosure
              mticket
              mflag
              is_preview
              inner_widget = do
    let user_id       = commentUser comment
        is_unapproved = not . commentIsApproved $ comment
        is_top_level  = commentIsTopLevel  comment
        is_even_depth = commentIsEvenDepth comment
        is_odd_depth  = commentIsOddDepth  comment

    -- TODO(mitchell): Lots of refactoring to lift this database hit up to the
    -- controller layer. This currently has horrible performance - a hit *per* comment!
    tags <- handlerToWidget $
        runDB $
          (sortAnnotTagsByName . (M.! comment_id)) <$>
            (fetchCommentCommentTagsDB comment_id >>= buildAnnotatedCommentTagsDB mviewer_id)

    $(widgetFile "comment")
