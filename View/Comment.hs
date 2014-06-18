module View.Comment where

import Import

import Data.Tree

import qualified Data.Foldable as F
import qualified Data.Map     as M
import qualified Data.List    as L

import Model.AnnotatedTag
import Model.User
import Model.Role
import Model.ClosureType
import Model.CollapseState

import Widgets.Markdown
import Widgets.Tag
import Widgets.Time

import Model.Markdown

countReplies :: [Tree a] -> Int
countReplies = sum . map (F.sum . fmap (const 1))

disabledCommentForm :: Form Markdown
disabledCommentForm = renderBootstrap3 $ areq snowdriftMarkdownField ("Reply" { fsAttrs = [("disabled",""), ("class","form-control")] }) Nothing

commentForm :: Maybe CommentId -> Maybe Markdown -> Form Markdown
commentForm parent content =
    let comment_label = if isJust parent then "Reply" else "New Topic"
     in renderBootstrap3 $ areq' snowdriftMarkdownField comment_label content

-- | commentWidget is for how each comment is rendered within whatever larger context it may have
commentWidget :: UTCTime                               -- ^ Timestamp.
              -> [Role]                                -- ^ The viewer's roles.
              -> Text                                  -- ^ Project handle.
              -> Text                                  -- ^ Wiki page name.
              -> Map UserId User                       -- ^ Users map.
              -> Int                                   -- ^ Max depth.
              -> Int                                   -- ^ Depth.
              -> [CommentClosure]                      -- ^ Earlier closures.
              -> Map CommentId CommentClosure          -- ^ Closure map.
              -> Map CommentId (Entity Ticket)         -- ^ Ticket map.
              -> Bool                                  -- ^ Show actions.
              -> Map TagId Tag                         -- ^ Tag map.
              -> Tree (Entity Comment)                 -- ^ Comment tree.
              -> Maybe Widget                          -- ^ Comment form.
              -> Widget
commentWidget now viewer_roles project_handle target users max_depth depth earlier_closures closure_map ticket_map show_actions tag_map tree mcomment_form = do
    mviewer           <- handlerToWidget maybeAuth
    maybe_route       <- handlerToWidget getCurrentRoute
    (comment_form, _) <- handlerToWidget $ generateFormPost $ commentForm Nothing Nothing

    let Entity comment_id comment = rootLabel tree
        children                  = subForest tree

        user_id             = commentUser comment
        user                = users M.! user_id
        author_name         = userPrintName $ Entity user_id user
        comment_time        = renderTime $ commentCreatedTs comment
        unapproved          = not . isJust $ commentModeratedTs comment

        is_top_level  = commentDepth comment == 0
        is_even_depth = not is_top_level && commentDepth comment `mod` 2 == 1
        is_odd_depth  = not is_top_level && not is_even_depth

        maybe_closure = M.lookup comment_id closure_map
        maybe_ticket  = M.lookup comment_id ticket_map
        empty_list    = []

        user_is_mod  = Moderator `elem` viewer_roles
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

-- | discussCommentWidget is for permalink views of particular comments
discussCommentWidget :: [Role]
                     -> Text
                     -> Text
                     -> Bool
                     -> Widget
                     -> Entity Comment
                     -> [Entity Comment]
                     -> Map UserId User
                     -> [CommentClosure]
                     -> Map CommentId CommentClosure
                     -> Map CommentId (Entity Ticket)
                     -> Bool
                     -> Map TagId Tag
                     -> Widget
discussCommentWidget roles project_handle target show_reply comment_form root rest users earlier_closures closure_map ticket_map show_actions tag_map = do
    now <- liftIO getCurrentTime

    let tree = buildCommentTree root rest
        comment = commentWidget
                      now
                      roles
                      project_handle
                      target
                      users
                      11
                      0
                      earlier_closures
                      closure_map
                      ticket_map
                      show_actions
                      tag_map
                      tree
                      (if show_reply then Just comment_form else Nothing)

    $(widgetFile "comment")

buildCommentTree :: Entity Comment -> [ Entity Comment ] -> Tree (Entity Comment)
buildCommentTree root rest =
    let treeOfList (node, items) =
            let has_parent p = (== Just (entityKey p)) . commentParent . entityVal
                list = dropWhile (not . has_parent node) items
                (children, rest') = span (has_parent node) list
                items' = map (, rest') children
             in (node, items')

     in unfoldTree treeOfList (root, rest)

