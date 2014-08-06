-- | Put all CSS for these widgets in templates/project_feed.cassius

module View.SnowdriftEvent where

import Import

renderCommentPostedOnWikiPageEvent :: CommentId -> Comment -> Entity WikiPage -> Widget
renderCommentPostedOnWikiPageEvent comment_id comment (Entity _ wiki_page) =
    [whamlet|
        <div>On #{wikiPageTarget wiki_page}: #{commentText comment}
            \ <a href=@{CommentDirectLinkR comment_id}>(permalink)
    |]

-- This should really *never* be called, but it's included in case of nuclear meltdown.
renderCommentPostedOnUnknownDiscussionEvent :: CommentId -> Comment -> Widget
renderCommentPostedOnUnknownDiscussionEvent comment_id comment =
    [whamlet|
        <div>#{commentText comment}
            \ <a href=@{CommentDirectLinkR comment_id}>(permalink)
    |]

renderCommentPendingEvent :: CommentId -> Comment -> Widget
renderCommentPendingEvent comment_id comment =
    [whamlet|
        <div>Comment pending: #{commentText comment}
            \ <a href=@{CommentDirectLinkR comment_id}>(permalink)
    |]

renderWikiEditEvent :: WikiEditId -> WikiEdit -> Entity WikiPage -> Widget
renderWikiEditEvent _ _ (Entity _ wiki_page) =
    [whamlet|
        <div>#{wikiPageTarget wiki_page} edit!
    |]
