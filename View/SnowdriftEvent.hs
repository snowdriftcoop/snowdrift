-- | Put all CSS for these widgets in templates/project_feed.cassius

module View.SnowdriftEvent where

import Import
import Model.User
import qualified Data.Map as M
import Data.Map ((!))
import Widgets.Time

renderCommentPostedOnWikiPageEvent :: Text -> CommentId -> Comment -> Entity WikiPage -> UserMap -> Widget
renderCommentPostedOnWikiPageEvent project_handle comment_id comment (Entity _ wiki_page) users_map = do
    let poster = fromMaybe
            (error "renderCommentPostedOnWikiPageEvent: poster not found in user map")
            (M.lookup (commentUser comment) users_map)
    [whamlet|
        <div .event>
            $maybe moderated_ts <- commentModeratedTs comment
                ^{renderTime moderated_ts}
            $nothing
                ^{renderTime $ commentCreatedTs comment}
            <a href=@{UserR (commentUser comment)}> #{userDisplayName (Entity (commentUser comment) poster)}
            posted a
            <a href=@{CommentDirectLinkR comment_id}>comment
            on the
            <a href=@{WikiR project_handle (wikiPageTarget wiki_page)}> #{wikiPageTarget wiki_page}
            wiki page: #{commentText comment}

    |]

-- This should really *never* be called, but it's included in case of nuclear meltdown.
renderCommentPostedOnUnknownDiscussionEvent :: CommentId -> Comment -> Widget
renderCommentPostedOnUnknownDiscussionEvent comment_id comment =
    [whamlet|
        <div .event>
            $maybe moderated_ts <- commentModeratedTs comment
                ^{renderTime moderated_ts}
            $nothing
                ^{renderTime $ commentCreatedTs comment}
            somehow, this
            <a href=@{CommentDirectLinkR comment_id}> comment
            was posted on an unknown discussion: #
            #{commentText comment}
    |]

renderCommentPendingEvent :: CommentId -> Comment -> UserMap -> Widget
renderCommentPendingEvent comment_id comment users_map = do
    let poster = fromMaybe
            (error "renderCommentPostedOnWikiPageEvent: poster not found in user map")
            (M.lookup (commentUser comment) users_map)
    [whamlet|
        <div .event>
            ^{renderTime $ commentCreatedTs comment}
            <a href=@{UserR (commentUser comment)}> #{userDisplayName (Entity (commentUser comment) poster)}
            posted a
            <a href=@{CommentDirectLinkR comment_id}> comment
            awaiting moderator approval:
            #{commentText comment}
    |]

renderWikiPageEvent :: Text -> WikiPageId -> WikiPage -> UserMap -> Widget
renderWikiPageEvent project_handle _ wiki_page users_map = do
--
-- The commented stuff here (and in the whamlet commented part)
-- is because there's no wikiPageUser yet and the
-- users_map is also not needed until this is active--
--    let editor = fromMaybe
--            (error "renderWikiPageEvent: wiki editor not found in user map")
--            (M.lookup (wikiPageUser wiki_page) users_map)
--
    [whamlet|
        <div .event>
            ^{renderTime $ wikiPageCreatedTs wiki_page}
            <!-- 
                <a href=@{UserR (wikiPageUser wiki_page)}>
                    #{userDisplayName (Entity (wikiPageUser wiki_page) editor)}
                -->
            made a new wiki page: #
            <a href=@{WikiR project_handle (wikiPageTarget wiki_page)}>#{wikiPageTarget wiki_page}
    |]

renderWikiEditEvent :: Text -> WikiEditId -> WikiEdit -> Entity WikiPage -> UserMap -> Widget
renderWikiEditEvent project_handle edit_id wiki_edit (Entity _ wiki_page) users_map = do
    let editor = fromMaybe
            (error "renderWikiEditEvent: wiki editor not found in user map")
            (M.lookup (wikiEditUser wiki_edit) users_map)
    [whamlet|
        <div .event>
            ^{renderTime $ wikiEditTs wiki_edit}
            <a href=@{UserR (wikiEditUser wiki_edit)}>
                #{userDisplayName (Entity (wikiEditUser wiki_edit) editor)}
            edited the
            <a href=@{WikiR project_handle (wikiPageTarget wiki_page)}> #{wikiPageTarget wiki_page}
            wiki page: #
            $maybe comment <- wikiEditComment wiki_edit
                #{comment}
            <a style="float:right" href="@{WikiEditR project_handle (wikiPageTarget wiki_page) edit_id}">
                see this edit version <!-- TODO: make this link to the diff instead -->
    |]

renderNewPledgeEvent :: SharesPledgedId -> SharesPledged -> UserMap -> Widget
renderNewPledgeEvent _ SharesPledged{..} users_map = do
    let pledger = users_map ! sharesPledgedUser
    [whamlet|
        <div .event>
            ^{renderTime sharesPledgedTs}
            <a href=@{UserR sharesPledgedUser}> #{userDisplayName (Entity sharesPledgedUser pledger)}
            pledged #{show sharesPledgedShares} new shares!
    |]

renderUpdatedPledgeEvent :: Int64 -> SharesPledgedId -> SharesPledged -> UserMap -> Widget
renderUpdatedPledgeEvent old_shares _ SharesPledged{..} users_map = do
    let pledger      = users_map ! sharesPledgedUser
        (verb, punc) = if old_shares < sharesPledgedShares
                           then ("increased", "!")
                           else ("decreased", ".") :: (Text, Text)
    [whamlet|
        <div .event>
            ^{renderTime sharesPledgedTs}
            <a href=@{UserR sharesPledgedUser}> #{userDisplayName (Entity sharesPledgedUser pledger)}
            #{verb} their pledge from #{show old_shares} to #{show sharesPledgedShares} shares#{punc}
    |]

renderDeletedPledgeEvent :: UTCTime -> UserId -> Int64 -> UserMap -> Widget
renderDeletedPledgeEvent ts user_id shares users_map = do
    let pledger = users_map ! user_id
    [whamlet|
        <div .event>
            ^{renderTime ts}
            <a href=@{UserR user_id}>#{userDisplayName (Entity user_id pledger)}
            withdrew their #{show shares}-share pledge.
    |]
