module View.Comment
    ( commentForm
    , commentFormWidget
    , commentForestWidget
    , commentTreeWidget
    , commentWidget
    , disabledCommentForm
    -- Comment action forms
    , claimCommentForm
    , unclaimCommentForm
    , watchCommentForm
    , unwatchCommentForm
    , closeCommentForm
    , commentNewTopicForm
    , commentReplyForm
    , createCommentTagForm
    , editCommentForm
    , flagCommentForm
    , generateFlagCommentForm
    , newCommentTagForm
    , rethreadCommentForm
    , retractCommentForm
    -- Comment action form widgets
    , approveCommentFormWidget
    , claimCommentFormWidget
    , closeCommentFormWidget
    , commentNewTopicFormWidget
    , commentReplyFormWidget
    , deleteCommentFormWidget
    , editCommentFormWidget
    , flagCommentFormWidget
    , rethreadCommentFormWidget
    , retractCommentFormWidget
    , unclaimCommentFormWidget
    , watchCommentFormWidget
    , unwatchCommentFormWidget
    -- Misc
    , orderingNewestFirst
    ) where

import Import

import Model.Comment
import Model.Comment.ActionPermissions
import Model.Comment.Routes
import Model.Tag
import Model.User
import Model.Markdown
import View.User
import Widgets.Markdown
import Widgets.Tag
import Widgets.Time

import qualified Data.List   as L
import qualified Data.Map    as M
import qualified Data.Text   as T
import           Data.Tree   (Forest, Tree(..))
import qualified Data.Tree   as Tree

disabledCommentForm :: Form Markdown
disabledCommentForm = renderBootstrap3 BootstrapBasicForm $ areq snowdriftMarkdownField ("Reply" { fsAttrs = [("disabled",""), ("class","form-control")] }) Nothing

closureForm :: SomeMessage App -> Maybe Markdown -> Form NewClosure
closureForm label message = renderBootstrap3 BootstrapBasicForm $ NewClosure <$> areq' snowdriftMarkdownField label message

commentForm :: SomeMessage App -> Maybe Markdown -> Form NewComment
commentForm label content = renderBootstrap3 BootstrapBasicForm $ NewComment
    <$> areq' snowdriftMarkdownField label content
    <*> pure VisPublic
    <*> areq' (selectField makeLanguageOptions) "Language" Nothing

--    TODO replace pure line above with below and uncomment where to activate private commenting
--    <*> (toVisibility <$> areq' checkBoxField "Private?" Nothing)
--  where
--    toVisibility True = VisPrivate
--    toVisibility _ = VisPublic

commentFormWidget :: Text -> SomeMessage App -> Maybe Markdown -> Widget
commentFormWidget post_text label content = commentFormWidget' True post_text (commentForm label content)

-- intentional duplication of commentFormWidget' because some aspects
-- of closing and other markdown aren't identical (such as marking privacy)
closureFormWidget' :: Text -> Form NewClosure -> Widget
closureFormWidget' post_text form = do
    (widget, enctype) <- handlerToWidget $ generateFormPost form
    [whamlet|
        <div>
            <form method="POST" enctype=#{enctype}>
                ^{widget}
                <button type="submit" name="mode" value="preview">preview
                <button type="submit" name="mode" value="post">#{post_text}
    |]

commentFormWidget' :: Bool -> Text -> Form a -> Widget
commentFormWidget' can_preview post_text form = do
    (widget, enctype) <- handlerToWidget $ generateFormPost form
    [whamlet|
        <div>
            <form method="POST" enctype=#{enctype}>
                ^{widget}

                $if can_preview
                    <button type="submit" name="mode" value="preview">preview

                <button type="submit" name="mode" value="post">#{post_text}
    |]

closeCommentForm    :: Maybe Markdown -> Form NewClosure
retractCommentForm  :: Maybe Markdown -> Form NewClosure

commentNewTopicForm ::               Form NewComment
commentReplyForm    ::               Form NewComment

editCommentForm     :: Markdown -> Language -> Form EditComment

closeCommentForm    = closureForm "Reason for closing:"
retractCommentForm  = closureForm "Reason for retracting:"

commentNewTopicForm = commentForm "New Topic" Nothing
commentReplyForm    = commentForm "Reply"     Nothing

editCommentForm content language =
    renderBootstrap3 BootstrapBasicForm $ EditComment
        <$> areq' snowdriftMarkdownField            "Edit"     (Just content)
        <*> areq' (selectField makeLanguageOptions) "Language" (Just language)

claimCommentFormWidget    :: Maybe (Maybe Text) -> Widget
closeCommentFormWidget    :: Maybe Markdown     -> Widget
commentNewTopicFormWidget ::                       Widget
commentReplyFormWidget    ::                       Widget
retractCommentFormWidget  :: Maybe Markdown     -> Widget
unclaimCommentFormWidget  :: Maybe (Maybe Text) -> Widget
watchCommentFormWidget    ::                       Widget
unwatchCommentFormWidget  ::                       Widget

editCommentFormWidget     :: Markdown -> Language -> Widget

closeCommentFormWidget    = closureFormWidget' "close" . closeCommentForm
retractCommentFormWidget  = closureFormWidget' "retract" . retractCommentForm

claimCommentFormWidget    = commentFormWidget' False "claim" . claimCommentForm
unclaimCommentFormWidget  = commentFormWidget' False "unclaim" . unclaimCommentForm
commentNewTopicFormWidget = commentFormWidget' True  "post" commentNewTopicForm
commentReplyFormWidget    = commentFormWidget' True  "post" commentReplyForm
watchCommentFormWidget    = commentFormWidget' False "watch" watchCommentForm
unwatchCommentFormWidget  = commentFormWidget' False "unwatch" unwatchCommentForm

editCommentFormWidget content language =
    commentFormWidget' True  "post" $ editCommentForm content language

approveCommentFormWidget :: Widget
approveCommentFormWidget =
    [whamlet|
        <form method="POST">
            <button type="submit" name="mode" value="post">approve
    |]

claimCommentForm :: Maybe (Maybe Text) -> Form (Maybe Text)
claimCommentForm = renderBootstrap3 BootstrapBasicForm . aopt' textField "Note (optional)"

unclaimCommentForm :: Maybe (Maybe Text) -> Form (Maybe Text)
unclaimCommentForm = renderBootstrap3 BootstrapBasicForm . aopt' textField "Note (optional)"

rethreadCommentForm :: Form (Text, Text)
rethreadCommentForm = renderBootstrap3 BootstrapBasicForm $ (,)
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

watchCommentForm :: Form ()
watchCommentForm = renderBootstrap3 BootstrapBasicForm $ pure ()

unwatchCommentForm :: Form ()
unwatchCommentForm = renderBootstrap3 BootstrapBasicForm $ pure ()

createCommentTagForm :: Form Text
createCommentTagForm = renderBootstrap3 BootstrapBasicForm $ areq' textField "Make a new tag:" Nothing

newCommentTagForm :: [Entity Tag] -> [Entity Tag] -> Form (Maybe [TagId], Maybe [TagId])
newCommentTagForm project_tags other_tags = renderBootstrap3 BootstrapBasicForm $ (,)
    -- <$> fmap (\(Entity tag_id tag) -> aopt checkBoxField (tag_id) (tagName tag)) (project_tags <> other_tags)
    <$> aopt (tagCloudFieldList project_tags) "Tags used elsewhere in this project:" Nothing
    <*> aopt (tagCloudFieldList other_tags) "Tags used in other projects:" Nothing
--    <*> areq hiddenField "" (Just "apply")
  where
    tagCloudFieldList tags =
        let toOption (Entity tag_id tag) = Option
                { optionDisplay = tagName tag
                , optionInternalValue = tag_id
                , optionExternalValue =
                    (\(PersistInt64 i) -> T.pack $ show i) $
                        toPersistValue tag_id
                }

            optlist = OptionList
                { olOptions = map toOption tags
                , olReadExternal = Just . key . PersistInt64 . read . T.unpack
                }
         in checkboxesField' (return optlist)

flagCommentForm :: Maybe (Maybe [FlagReason]) -> Maybe (Maybe Markdown) -> Form (Maybe [FlagReason], Maybe Markdown)
flagCommentForm def_reasons def_message = renderBootstrap3 BootstrapBasicForm $ (,) <$> flagReasonsForm <*> additionalCommentsForm
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
            <button type="submit" name="mode" value="preview">preview
            <button type="submit" name="mode" value="post">flag
    |]

generateFlagCommentForm :: Maybe (Maybe [FlagReason]) -> Maybe (Maybe Markdown) -> Widget
generateFlagCommentForm reasons message = do
    (form, _) <- handlerToWidget (generateFormPost $ flagCommentForm reasons message)
    [whamlet|
        <h4>Code of Conduct Violation(s):
        ^{form}
    |]
    toWidget [cassius|
        .preview-action-button[type=submit]
            background : dark-red
            background-image : linear-gradient(#ee2700, #bd1000)
            border-color: #a5022a

        .preview-action-button[type=submit]:hover, .preview-action-button[type=submit]:focus, .preview-action-button[type=submit]:active
            background : red
            background-image : linear-gradient(#d22935, #a5022a)
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
    newest = L.maximumBy (compare `on` timestamp) . Tree.flatten

    timestamp :: Entity Comment -> UTCTime
    timestamp = commentCreatedTs . entityVal

expandCommentWidget :: Int -> MaxDepth -> Widget
expandCommentWidget num_replies new_max_depth = do
    Just cur_route <- getCurrentRoute
    let new_route = case new_max_depth of
                        NoMaxDepth -> (cur_route, [])
                        MaxDepth n -> (cur_route, [("maxdepth", T.pack (show n))])
    [whamlet|
        <a .expand href="@?{new_route}">
            #{num_replies} more #{plural num_replies "reply" "replies"}
    |]

-- | An entire comment forest.
commentForestWidget
        :: Forest (Entity Comment)
        -> Maybe UserId             -- ^ Viewer.
        -> CommentRoutes
        -> MakeActionPermissionsMap
        -> [CommentClosing]         -- ^ Earlier closures.
        -> [CommentRetracting]      -- ^ Earlier retracts.
        -> Map UserId User
        -> Map CommentId CommentClosing
        -> Map CommentId CommentRetracting
        -> Map CommentId (Entity Ticket)
        -> Map CommentId TicketClaiming
        -> Map CommentId (CommentFlagging, [FlagReason])
        -> Bool                     -- ^ Is preview?
        -> MaxDepth                 -- ^ Max depth.
        -> Int                      -- ^ Depth.
        -> Widget                   -- ^ Widget to display under each root comment.
        -> Widget
commentForestWidget
        comment_forest
        mviewer_id
        comment_routes
        make_action_permissions_map
        earlier_closures
        earlier_retracts
        user_map
        close_map
        retract_map
        ticket_map
        claim_map
        flag_map
        is_preview
        max_depth
        depth
        widget_under_root_comment = do
    action_permissions_map <- handlerToWidget (make_action_permissions_map (concatMap Tree.flatten comment_forest))
    forM_ comment_forest $ \comment_tree ->
        commentTreeWidget'
          comment_tree
          mviewer_id
          comment_routes
          action_permissions_map
          earlier_closures
          earlier_retracts
          user_map
          close_map
          retract_map
          ticket_map
          claim_map
          flag_map
          is_preview
          max_depth
          depth
          widget_under_root_comment

-- | An entire comment tree.
commentTreeWidget
        :: Tree (Entity Comment)
        -> Maybe UserId             -- ^ Viewer.
        -> CommentRoutes
        -> MakeActionPermissionsMap
        -> [CommentClosing]         -- ^ Earlier closures.
        -> [CommentRetracting]      -- ^ Earlier retracts.
        -> Map UserId User
        -> Map CommentId CommentClosing
        -> Map CommentId CommentRetracting
        -> Map CommentId (Entity Ticket)
        -> Map CommentId TicketClaiming
        -> Map CommentId (CommentFlagging, [FlagReason])
        -> Bool                     -- ^ Is preview?
        -> MaxDepth
        -> Int                      -- ^ Depth.
        -> Widget                   -- ^ Form to display under the root comment.
        -> Widget
commentTreeWidget tree = commentForestWidget [tree]

-- | Helper function for commentForestWidget/commentTreeWidget that takes an
-- ActionPermissionsMap (as opposed to a MakeActionPermissionsMap). Unexported.
commentTreeWidget'
        :: Tree (Entity Comment)
        -> Maybe UserId          -- ^ Viewer.
        -> CommentRoutes
        -> ActionPermissionsMap
        -> [CommentClosing]      -- ^ Earlier closures.
        -> [CommentRetracting]   -- ^ Earlier retracts.
        -> Map UserId User
        -> Map CommentId CommentClosing
        -> Map CommentId CommentRetracting
        -> Map CommentId (Entity Ticket)
        -> Map CommentId TicketClaiming
        -> Map CommentId (CommentFlagging, [FlagReason])
        -> Bool                  -- ^ Is preview?
        -> MaxDepth
        -> Int                   -- ^ Depth.
        -> Widget                -- ^ Form to display under the root comment.
        -> Widget
commentTreeWidget'
        (Node root_entity@(Entity root_id root) children)
        mviewer_id
        comment_routes
        action_permissions_map
        earlier_closures
        earlier_retracts
        user_map
        close_map
        retract_map
        ticket_map
        claim_map
        flag_map
        is_preview
        max_depth
        depth
        form_under_root_comment = do

    let num_children = length children
        inner_widget =
            form_under_root_comment <>
            if MaxDepth depth >= max_depth && num_children > 0
                then expandCommentWidget num_children (addMaxDepth max_depth 2) -- FIXME: arbitrary '2' here
                else forM_ children $ \child ->
                         commentTreeWidget'
                           child
                           mviewer_id
                           comment_routes
                           action_permissions_map
                           [] -- don't want to show earlier closures on *all* comments, just the first one.
                           [] -- same for earlier retracts
                           user_map
                           close_map
                           retract_map
                           ticket_map
                           claim_map
                           flag_map
                           is_preview
                           max_depth
                           (depth+1)
                           mempty

    commentWidget
        root_entity
        mviewer_id
        comment_routes
        (lookupErr "comment id missing from action permissions map" root_id action_permissions_map)
        earlier_closures
        earlier_retracts
        (lookupErr "comment user missing from user map" (commentUser root) user_map)
        (M.lookup root_id close_map)
        (M.lookup root_id retract_map)
        (M.lookup root_id ticket_map)
        (M.lookup root_id claim_map)
        (M.lookup root_id flag_map)
        is_preview
        inner_widget

-- | A "single" comment, which also displays an 'inner widget' inside of it.
-- The reason this can't be made more modular is the HTML for nested comments
-- requires us to render the entire tree (can't close the parent comment's div
-- before the children comments).
--
-- Note this widget has NO CSS.
commentWidget :: Entity Comment                        -- ^ Comment.
              -> Maybe UserId                          -- ^ Viewer.
              -> CommentRoutes                         -- ^ Comment routes.
              -> CommentActionPermissions              -- ^ Permissions for comment actions.
              -> [CommentClosing]                      -- ^ Earlier closures.
              -> [CommentRetracting]                   -- ^ Earlier retracts.
              -> User                                  -- ^ Comment poster.
              -> Maybe CommentClosing                  -- ^ Is this closed?
              -> Maybe CommentRetracting               -- ^ Is this retracted?
              -> Maybe (Entity Ticket)                 -- ^ Is this a ticket?
              -> Maybe TicketClaiming                  -- ^ Is this ticket claimed?
              -> Maybe (CommentFlagging, [FlagReason]) -- ^ Is this flagged?
              -> Bool                                  -- ^ Is this a preview?
              -> Widget                                -- ^ Inner widget (children comments, 'expand' link, reply box, etc)
              -> Widget
commentWidget (Entity comment_id comment)
              mviewer_id
              CommentRoutes{..}
              CommentActionPermissions{..}
              earlier_closures
              earlier_retracts
              user
              mclosure
              mretract
              mticket
              mclaim
              mflag
              is_preview
              inner_widget = do
    let user_id       = commentUser comment
        is_unapproved = not . commentIsApproved $ comment
        is_top_level  = commentIsTopLevel  comment
        is_even_depth = commentIsEvenDepth comment
        is_odd_depth  = commentIsOddDepth  comment
        is_private    = commentIsPrivate   comment

    -- TODO: Lots of refactoring to lift this database hit up to the
    -- controller layer. This currently has horrible performance - a hit *per* comment!
    tags <- handlerToWidget $ runDB $
        maybe [] sortAnnotTagsByName .
          M.lookup comment_id <$>
            (fetchCommentCommentTagsDB comment_id >>= buildAnnotatedCommentTagsDB mviewer_id)

    user_map <- case mclaim of
        Nothing -> return M.empty
        Just claim -> do
            let claiming_user_id = ticketClaimingUser claim
            Just claiming_user <- runDB $ get claiming_user_id
            return $ M.singleton claiming_user_id claiming_user

    let ticket_str = case fmap (toPersistValue . entityKey) mticket of
            Just (PersistInt64 tid) -> T.pack $ show tid
            _ -> "???"

        prettyTicketLine line =
            let pretty title = "<div class='ticket-title'>SD-" <> ticket_str <> ": " <> title <> "</div>"
             in return $ maybe line pretty $ T.stripPrefix "ticket: " line

        commentTextTransform = prettyTicketLine

    $(whamletFile "templates/comment.hamlet")
