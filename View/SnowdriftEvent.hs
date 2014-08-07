-- | Put all CSS for these widgets in templates/project_feed.cassius

module View.SnowdriftEvent where

import Import

import Model.User

import Data.Map ((!))

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

renderWikiPageEvent :: WikiPageId -> WikiPage -> Widget
renderWikiPageEvent wiki_page_id wiki_page =
    [whamlet|
        <div>Wiki page: #{wikiPageTarget wiki_page}
    |]

renderWikiEditEvent :: WikiEditId -> WikiEdit -> Entity WikiPage -> Widget
renderWikiEditEvent _ _ (Entity _ wiki_page) =
    [whamlet|
        <div>#{wikiPageTarget wiki_page} edit!
    |]

renderNewPledgeEvent :: PledgeId -> Pledge -> UserMap -> Widget
renderNewPledgeEvent pledge_id Pledge{..} users_map = do
    let pledger = users_map ! pledgeUser
    [whamlet|
        <div>#{userPrintName (Entity pledgeUser pledger)} pledged #{show pledgeShares} new shares!
    |]

renderUpdatedPledgeEvent :: Int64 -> PledgeId -> Pledge -> UserMap -> Widget
renderUpdatedPledgeEvent old_shares pledge_id Pledge{..} users_map = do
    let pledger = users_map ! pledgeUser
        direction_text = if old_shares < pledgeShares then "down " else "up " :: Text
    [whamlet|
        <div>#{userPrintName (Entity pledgeUser pledger)} pledged #{show pledgeShares} shares! (#{direction_text} from #{show old_shares})
    |]

renderDeletedPledgeEvent :: UserId -> Int64 -> UserMap -> Widget
renderDeletedPledgeEvent user_id shares users_map = do
    let pledger = users_map ! user_id
    [whamlet|
        <div>#{userPrintName (Entity user_id pledger)} withdrew #{show shares}.
    |]
