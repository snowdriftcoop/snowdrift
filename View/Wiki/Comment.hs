-- | Display Comments on WikiPages.
module View.Wiki.Comment
    ( makeWikiPageCommentForestWidget
    , makeWikiPageCommentForestWidget'
    , makeWikiPageCommentTreeWidget
    , makeWikiPageCommentTreeWidget'
    ) where

import Import

import Model.Comment
import Model.Wiki.Comment
import Model.Wiki.Comment.Sql
import View.Comment

import Data.Tree (Forest, Tree)

makeWikiPageCommentForestWidget
        :: Maybe (Entity User)
        -> ProjectId
        -> Text             -- ^ Project handle.
        -> Text             -- ^ Wiki page.
        -> [Entity Comment] -- ^ Root comments.
        -> CommentMods      -- ^ Comment modifications.
        -> Handler MaxDepth -- ^ Max depth getter.
        -> Bool             -- ^ Is this a preview?
        -> Widget           -- ^ Widget to display under each root comment.
        -> Handler (Widget, Forest (Entity Comment))
makeWikiPageCommentForestWidget
        muser
        project_id
        project_handle
        target
        comments
        comment_mods
        get_max_depth
        is_preview
        widget_under_root_comment = do
    max_depth <- get_max_depth
    let has_permission = exprCommentWikiPagePermissionFilter (entityKey <$> muser) (val project_id)
    makeCommentForestWidget
      comments
      muser
      has_permission
      comment_mods
      (wikiPageCommentRoutes project_handle target)
      (makeWikiPageCommentActionPermissions project_handle target)
      max_depth
      is_preview
      widget_under_root_comment

makeWikiPageCommentForestWidget'
        :: Maybe (Entity User)
        -> ProjectId
        -> Text
        -> Text
        -> [Entity Comment]
        -> CommentMods
        -> Handler MaxDepth
        -> Bool
        -> Widget
        -> Handler Widget
makeWikiPageCommentForestWidget' a b c d e f g h i = fst <$> makeWikiPageCommentForestWidget a b c d e f g h i

-- | Make a Comment tree Widget suitable for display on a WikiPage. Specifically,
-- a WikiPage has its own permissions, show-actions logic, and URLs, distinct
-- from other pages' Comment trees.
makeWikiPageCommentTreeWidget
        :: Maybe (Entity User)
        -> ProjectId
        -> Text             -- ^ Project handle.
        -> Text             -- ^ Wiki page.
        -> Entity Comment   -- ^ Root comment.
        -> CommentMods      -- ^ Comment modifications.
        -> Handler MaxDepth -- ^ Max depth getter.
        -> Bool             -- ^ Is this a preview?
        -> Widget           -- ^ Widget to display under root comment.
        -> Handler (Widget, Tree (Entity Comment))
makeWikiPageCommentTreeWidget
        muser
        project_id
        project_handle
        target
        comment
        comment_mods
        get_max_depth
        is_preview
        widget_under_root_comment = do
    max_depth <- get_max_depth
    let has_permission = exprCommentWikiPagePermissionFilter (entityKey <$> muser) (val project_id)
    makeCommentTreeWidget
      comment
      muser
      has_permission
      comment_mods
      (wikiPageCommentRoutes project_handle target)
      (makeWikiPageCommentActionPermissions project_handle target)
      max_depth
      is_preview
      widget_under_root_comment

-- | Like makeWikiPageCommentTreeWidget, but don't return the Tree of Comments, just the Widget.
makeWikiPageCommentTreeWidget' :: Maybe (Entity User)
                               -> ProjectId
                               -> Text
                               -> Text
                               -> Entity Comment
                               -> CommentMods
                               -> Handler MaxDepth
                               -> Bool
                               -> Widget
                               -> Handler Widget
makeWikiPageCommentTreeWidget' a b c d e f g h i = fst <$> makeWikiPageCommentTreeWidget a b c d e f g h i
