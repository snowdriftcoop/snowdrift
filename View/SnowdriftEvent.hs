-- | Put all CSS for these widgets in templates/project_feed.cassius

module View.SnowdriftEvent where

import Import

import Model.Comment
import Model.Comment.ActionPermissions
import Model.Comment.Routes
import Model.User
import View.Comment
import Widgets.Time

import qualified Data.Map as M

renderProjectCommentPostedEvent
        :: CommentId
        -> Comment
        -> Project
        -> Maybe UserId
        -> ActionPermissionsMap
        -> Map CommentId [CommentClosure]
        -> UserMap
        -> ClosureMap
        -> TicketMap
        -> FlagMap
        -> Widget
renderProjectCommentPostedEvent
        comment_id
        comment
        Project{..}
        mviewer_id
        action_permissions_map
        earlier_closures_map
        user_map
        closure_map
        ticket_map
        flag_map = do

    let comment_widget =
            commentWidget
              (Entity comment_id comment)
              mviewer_id
              (projectCommentRoutes projectHandle)
              (lookupErr "renderProjectCommentPostedEvent: comment id missing from permissions map"
                         comment_id
                         action_permissions_map)
              (M.findWithDefault [] comment_id earlier_closures_map)
              (lookupErr "renderProjectCommentPostedEvent: comment user missing from user map"
                         (commentUser comment)
                         user_map)
              (M.lookup comment_id closure_map)
              (M.lookup comment_id ticket_map)
              (M.lookup comment_id flag_map)
              False
              mempty

    [whamlet|
        <div .event>
            On
            <a href=@{ProjectR projectHandle}>#{projectName}#
            :

            ^{comment_widget}
    |]

renderWikiPageCommentPostedEvent
        :: CommentId
        -> Comment
        -> Entity WikiPage
        -> Text
        -> Maybe UserId
        -> ActionPermissionsMap
        -> Map CommentId [CommentClosure]
        -> UserMap
        -> ClosureMap
        -> TicketMap
        -> FlagMap
        -> Widget
renderWikiPageCommentPostedEvent
        comment_id
        comment
        (Entity _ wiki_page)
        project_handle
        mviewer_id
        action_permissions_map
        earlier_closures_map
        user_map
        closure_map
        ticket_map
        flag_map = do

    let target = wikiPageTarget wiki_page
        comment_widget =
            commentWidget
              (Entity comment_id comment)
              mviewer_id
              (wikiPageCommentRoutes project_handle target)
              (lookupErr "renderWikiPageCommentPostedEvent: comment id missing from permissions map"
                         comment_id
                         action_permissions_map)
              (M.findWithDefault [] comment_id earlier_closures_map)
              (lookupErr "renderWikiPageCommentPostedEvent: comment user missing from user map"
                         (commentUser comment)
                         user_map)
              (M.lookup comment_id closure_map)
              (M.lookup comment_id ticket_map)
              (M.lookup comment_id flag_map)
              False
              mempty

    [whamlet|
        <div .event>
            On the
            <a href=@{WikiR project_handle target}>#{target}
            wiki page:

            ^{comment_widget}
    |]

-- This should really *never* be called, but it's included in case of nuclear meltdown.
renderCommentPostedOnUnknownDiscussionEvent :: CommentId -> Comment -> Widget
renderCommentPostedOnUnknownDiscussionEvent comment_id comment =
    [whamlet|
        <div .event>
            $maybe approved_ts <- commentApprovedTs comment
                ^{renderTime approved_ts}
            $nothing
                ^{renderTime $ commentCreatedTs comment}
            somehow, this
            <a href=@{CommentDirectLinkR comment_id}> comment
            was posted on an unknown discussion: #
            #{commentText comment}
    |]

renderCommentPendingEvent :: CommentId -> Comment -> UserMap -> Widget
renderCommentPendingEvent comment_id comment user_map = do
    let poster = lookupErr "renderCommentPendingEvent: poster not found in user map" (commentUser comment) user_map
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
renderWikiPageEvent project_handle _ wiki_page _ = do
-- TODO(aaron)
-- The commented stuff here (and in the whamlet commented part)
-- is because there's no wikiPageUser yet and the
-- user_map is also not needed until this is active--
--    let editor = fromMaybe
--            (error "renderWikiPageEvent: wiki editor not found in user map")
--            (M.lookup (wikiPageUser wiki_page) user_map)
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

renderWikiEditEvent :: Text -> WikiEditId -> WikiEdit -> Map WikiPageId WikiPage -> UserMap -> Widget
renderWikiEditEvent project_handle edit_id wiki_edit wiki_page_map user_map = do
    let editor    = lookupErr "renderWikiEditEvent: wiki editor not found in user map"    (wikiEditUser wiki_edit) user_map
        wiki_page = lookupErr "renderWikiEditEvent: wiki page not found in wiki page map" (wikiEditPage wiki_edit) wiki_page_map
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
renderNewPledgeEvent _ SharesPledged{..} user_map = do
    let pledger = lookupErr "renderNewPledgeEvent: pledger not found in user map" sharesPledgedUser user_map
    [whamlet|
        <div .event>
            ^{renderTime sharesPledgedTs}
            <a href=@{UserR sharesPledgedUser}> #{userDisplayName (Entity sharesPledgedUser pledger)}
            pledged #{show sharesPledgedShares} new shares!
    |]

renderUpdatedPledgeEvent :: Int64 -> SharesPledgedId -> SharesPledged -> UserMap -> Widget
renderUpdatedPledgeEvent old_shares _ SharesPledged{..} user_map = do
    let pledger = lookupErr "renderUpdatedPledgeEvent: pledger not found in user map" sharesPledgedUser user_map
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
renderDeletedPledgeEvent ts user_id shares user_map = do
    let pledger = lookupErr "renderDeletedPledgeEvent: pledger not found in user map" user_id user_map
    [whamlet|
        <div .event>
            ^{renderTime ts}
            <a href=@{UserR user_id}>#{userDisplayName (Entity user_id pledger)}
            withdrew their #{show shares}-share pledge.
    |]
