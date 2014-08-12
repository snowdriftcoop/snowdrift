-- | Put all CSS for these widgets in templates/project_feed.cassius

module View.SnowdriftEvent where

import Import

import Model.User

import Data.Map ((!))

renderCommentPostedOnWikiPageEvent :: CommentId -> Comment -> Entity WikiPage -> Widget
renderCommentPostedOnWikiPageEvent comment_id comment (Entity _ wiki_page) =
    [whamlet|
        <div>On /w/#{wikiPageTarget wiki_page}: #{commentText comment}
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

renderWikiPageEvent :: WikiPageId -> WikiPage -> Widget
renderWikiPageEvent _ wiki_page =
    [whamlet|
        <div>Wiki page: #{wikiPageTarget wiki_page}
    |]

renderWikiEditEvent :: WikiEditId -> WikiEdit -> Entity WikiPage -> Widget
renderWikiEditEvent _ _ (Entity _ wiki_page) =
    [whamlet|
        <div>#{wikiPageTarget wiki_page} edit!
    |]

renderNewPledgeEvent :: SharesPledgedId -> SharesPledged -> UserMap -> Widget
renderNewPledgeEvent _ SharesPledged{..} users_map = do
    let pledger = users_map ! sharesPledgedUser
    [whamlet|
        <div>#{userDisplayName (Entity sharesPledgedUser pledger)} pledged #{show sharesPledgedShares} new shares!
    |]

renderUpdatedPledgeEvent :: Int64 -> SharesPledgedId -> SharesPledged -> UserMap -> Widget
renderUpdatedPledgeEvent old_shares _ SharesPledged{..} users_map = do
    let pledger      = users_map ! sharesPledgedUser
        (verb, punc) = if old_shares < sharesPledgedShares
                           then ("increased", "!")
                           else ("decreased", ".") :: (Text, Text)
    [whamlet|
        <div>#{userDisplayName (Entity sharesPledgedUser pledger)} #{verb} their pledge from #{show old_shares} to #{show sharesPledgedShares} shares#{punc}
    |]

renderDeletedPledgeEvent :: UserId -> Int64 -> UserMap -> Widget
renderDeletedPledgeEvent user_id shares users_map = do
    let pledger = users_map ! user_id
    [whamlet|
        <div>#{userDisplayName (Entity user_id pledger)} withdrew their #{show shares}-share pledge.
    |]
