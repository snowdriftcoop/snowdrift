-- | Put all CSS for these widgets in templates/project_feed.cassius

module View.SnowdriftEvent where

import Import

import Model.Comment
import Model.Comment.ActionPermissions
import Model.Comment.Routes
import Model.User
import View.Comment
import Widgets.Time

import qualified Data.Map   as M
import qualified Data.Text  as T

renderCommentPostedEvent
        :: CommentId
        -> Comment
        -> Maybe UserId
        -> Text
        -> Map DiscussionId DiscussionOn
        -> ActionPermissionsMap
        -> Map CommentId [CommentClosing]
        -> Map CommentId [CommentRetracting]
        -> Map UserId User
        -> Map CommentId CommentClosing
        -> Map CommentId CommentRetracting
        -> Map CommentId (Entity Ticket)
        -> Map CommentId TicketClaiming
        -> Map CommentId (CommentFlagging, [FlagReason])
        -> Widget
renderCommentPostedEvent
        comment_id
        comment
        mviewer_id
        project_handle
        discussion_map
        action_permissions_map
        earlier_closures_map
        earlier_retracts_map
        user_map
        closure_map
        retract_map
        ticket_map
        claim_map
        flag_map = do

    let action_permissions = lookupErr "renderCommentPostedEvent: comment id missing from permissions map"
                                       comment_id
                                       action_permissions_map

        user               = lookupErr "renderCommentPostedEvent: comment user missing from user map"
                                       (commentUser comment)
                                       user_map

        discussion         = lookupErr "renderCommentPostedEvent: discussion id not found in map"
                                      (commentDiscussion comment)
                                      discussion_map

        (routes, feed_item_widget) = case discussion of
            DiscussionOnProject (Entity _ Project{..}) ->
                (projectCommentRoutes projectHandle, [whamlet|
                    <div .event>
                        On
                        <a href=@{ProjectDiscussionR projectHandle}>#{projectName}#
                        :

                        ^{comment_widget}
                |])

            DiscussionOnWikiPage (Entity _ WikiTarget{..}) ->
                (wikiPageCommentRoutes project_handle wikiTargetLanguage wikiTargetTarget, [whamlet|
                    <div .event>
                        On the
                        <a href=@{WikiDiscussionR project_handle wikiTargetLanguage wikiTargetTarget}>#{wikiTargetTarget}
                        wiki page:

                        ^{comment_widget}
                |])

            DiscussionOnUser (Entity user_id _) ->
                (userCommentRoutes user_id, [whamlet|
                    <div .event>
                        On your user discussion page:
                        ^{comment_widget}
                |])

            DiscussionOnBlogPost (Entity _ BlogPost{..}) ->
                (blogPostCommentRoutes project_handle blogPostHandle, [whamlet|
                    <div .event>
                        On blog post
                        <a href=@{BlogPostDiscussionR project_handle blogPostHandle}>#{blogPostTitle}
                        :

                        ^{comment_widget}
                |])

        comment_widget =
            commentWidget
              (Entity comment_id comment)
              mviewer_id
              routes
              action_permissions
              (M.findWithDefault [] comment_id earlier_closures_map)
              (M.findWithDefault [] comment_id earlier_retracts_map)
              user
              (M.lookup comment_id closure_map)
              (M.lookup comment_id retract_map)
              (M.lookup comment_id ticket_map)
              (M.lookup comment_id claim_map)
              (M.lookup comment_id flag_map)
              False
              mempty

    feed_item_widget

renderCommentPendingEvent :: CommentId -> Comment -> UserMap -> Widget
renderCommentPendingEvent comment_id comment user_map = do
    let poster = lookupErr "renderCommentPendingEvent: poster not found in user map" (commentUser comment) user_map
    [whamlet|
        <div .event>
            ^{renderTime $ commentCreatedTs comment}
            <a href=@{UserR (commentUser comment)}> #{userDisplayName (Entity (commentUser comment) poster)}
            posted a
            <a href=@{CommentDirectLinkR comment_id}> comment
            awaiting moderator approval: #{commentText comment}
    |]

renderCommentRethreadedEvent :: Rethread -> UserMap -> Widget
renderCommentRethreadedEvent Rethread{..} user_map = do
    langs <- handlerToWidget getLanguages

    (Just old_route, Just new_route) <- handlerToWidget $ runDB $ (,)
        <$> makeCommentRouteDB langs rethreadOldComment
        <*> makeCommentRouteDB langs rethreadNewComment

    let user = lookupErr "renderCommentRethreadedEvent: rethreader not found in user map" rethreadModerator user_map

    [whamlet|
        <div .event>
            ^{renderTime rethreadTs}
            <a href=@{UserR rethreadModerator}> #{userDisplayName (Entity rethreadModerator user)}
            rethreaded a comment from
            <del>@{old_route}
            to
            <a href=@{new_route}>@{new_route}#
            : #{rethreadReason}
    |]

renderCommentClosedEvent :: CommentClosing -> UserMap -> Map CommentId (Entity Ticket) -> Widget
renderCommentClosedEvent CommentClosing{..} user_map ticket_map = do
    let user = lookupErr "renderCommentClosedEvent: closing user not found in user map" commentClosingClosedBy user_map

    case M.lookup commentClosingComment ticket_map of
        Just (Entity ticket_id Ticket{..}) -> do
            let ticket_str = case toPersistValue ticket_id of
                    PersistInt64 tid -> T.pack $ show tid
                    _ -> "<malformed key>"

            [whamlet|
                <div .event>
                    ^{renderTime commentClosingTs}
                    <a href=@{UserR commentClosingClosedBy}> #{userDisplayName (Entity commentClosingClosedBy user)}
                    closed ticket
                    <a href=@{CommentDirectLinkR commentClosingComment}>
                        <div .ticket-title>SD-#{ticket_str}: #{ticketName}
            |]

        Nothing ->
            [whamlet|
                <div .event>
                    ^{renderTime commentClosingTs}
                    <a href=@{UserR commentClosingClosedBy}> #{userDisplayName (Entity commentClosingClosedBy user)}
                    closed
                    <a href=@{CommentDirectLinkR commentClosingComment}>
                        comment thread
            |]

renderTicketClaimedEvent :: Either (TicketClaimingId, TicketClaiming) (TicketOldClaimingId, TicketOldClaiming) -> UserMap -> Map CommentId (Entity Ticket) -> Widget
renderTicketClaimedEvent (Left (_, TicketClaiming{..})) user_map ticket_map = do
    let user = lookupErr "renderTicketClaimedEvent: claiming user not found in user map" ticketClaimingUser user_map
        Entity ticket_id Ticket{..} = lookupErr "renderTicketClaimedEvent: ticket not found in map" ticketClaimingTicket ticket_map

        ticket_str = case toPersistValue ticket_id of
            PersistInt64 tid -> T.pack $ show tid
            _ -> "<malformed key>"

    [whamlet|
        <div .event>
            ^{renderTime ticketClaimingTs}
            <a href=@{UserR ticketClaimingUser}> #{userDisplayName (Entity ticketClaimingUser user)}
            claimed ticket
            <a href=@{CommentDirectLinkR ticketClaimingTicket}>
                <div .ticket-title>SD-#{ticket_str}: #{ticketName}
    |]

renderTicketClaimedEvent (Right (_, TicketOldClaiming{..})) user_map ticket_map = do
    let user = lookupErr "renderTicketClaimedEvent: claiming user not found in user map" ticketOldClaimingUser user_map
        Entity ticket_id Ticket{..} = lookupErr "renderTicketClaimedEvent: ticket not found in map" ticketOldClaimingTicket ticket_map

        ticket_str = case toPersistValue ticket_id of
            PersistInt64 tid -> T.pack $ show tid
            _ -> "<malformed key>"

    [whamlet|
        <div .event>
            ^{renderTime ticketOldClaimingClaimTs}
            <a href=@{UserR ticketOldClaimingUser}> #{userDisplayName (Entity ticketOldClaimingUser user)}
            claimed ticket
            <a href=@{CommentDirectLinkR ticketOldClaimingTicket}>
                <div .ticket-title>SD-#{ticket_str}: #{ticketName}
    |]

renderTicketUnclaimedEvent :: TicketOldClaiming -> UserMap -> Map CommentId (Entity Ticket) -> Widget
renderTicketUnclaimedEvent TicketOldClaiming{..} _ ticket_map = do
    let Entity ticket_id Ticket{..} = lookupErr "renderTicketUnclaimedEvent: ticket not found in map" ticketOldClaimingTicket ticket_map
        ticket_str = case toPersistValue ticket_id of
            PersistInt64 tid -> T.pack $ show tid
            _ -> "<malformed key>"

    [whamlet|
        <div .event>
            ^{renderTime ticketOldClaimingClaimTs}
            Claim released, ticket available:
            <a href=@{CommentDirectLinkR ticketOldClaimingTicket}>
                <div .ticket-title>SD-#{ticket_str}: #{ticketName}
    |]

renderWikiPageEvent :: Text -> WikiPageId -> WikiPage -> UserMap -> Widget
renderWikiPageEvent project_handle wiki_page_id wiki_page _ = do
-- TODO(aaron)
-- The commented stuff here (and in the whamlet commented part)
-- is because there's no wikiPageUser and the
-- user_map is also not needed until that is active--
--    let editor = fromMaybe
--            (error "renderWikiPageEvent: wiki editor not found in user map")
--            (M.lookup (wikiPageUser wiki_page) user_map)
--perhaps instead of a wikiPageUser, we should just figure out how to pull
--the user from the first wiki edit for the event of new pages

-- TODO: pick language correctly
    [Entity _ wiki_target] <- runDB $ select $ from $ \wt -> do
        where_ $ wt ^. WikiTargetPage ==. val wiki_page_id
        limit 1
        return wt

    [whamlet|
        <div .event>
            ^{renderTime $ wikiPageCreatedTs wiki_page}
            <!--
                <a href=@{UserR (wikiPageUser wiki_page)}>
                    #{userDisplayName (Entity (wikiPageUser wiki_page) editor)}
                -->
            made a new wiki page: #
            <a href=@{WikiR project_handle (wikiTargetLanguage wiki_target) (wikiTargetTarget wiki_target)}>#{wikiTargetTarget wiki_target}
    |]

renderWikiEditEvent :: Text -> WikiEditId -> WikiEdit -> Map WikiPageId WikiTarget -> UserMap -> Widget
renderWikiEditEvent project_handle edit_id wiki_edit wiki_target_map user_map = do
    let editor      = lookupErr "renderWikiEditEvent: wiki editor not found in user map"         (wikiEditUser wiki_edit) user_map
        wiki_target = lookupErr "renderWikiEditEvent: wiki page id not found in wiki target map" (wikiEditPage wiki_edit) wiki_target_map
    [whamlet|
        <div .event>
            ^{renderTime $ wikiEditTs wiki_edit}
            <a href=@{UserR (wikiEditUser wiki_edit)}>
                #{userDisplayName (Entity (wikiEditUser wiki_edit) editor)}
            edited the
            <a href=@{WikiR project_handle (wikiTargetLanguage wiki_target) (wikiTargetTarget wiki_target)}> #{wikiTargetTarget wiki_target}
            wiki page: #
            $maybe comment <- wikiEditComment wiki_edit
                #{comment}
            <br>
            <a href="@{WikiEditR project_handle (wikiTargetLanguage wiki_target) (wikiTargetTarget wiki_target) edit_id}">
                see this edit version <!-- TODO: make this link to the diff instead -->
    |]

renderBlogPostEvent :: BlogPost -> Widget
renderBlogPostEvent (BlogPost {..}) = do
    maybe_project <- handlerToWidget $ runYDB $ get blogPostProject

    [whamlet|
        <div .event>
            ^{renderTime blogPostTs}
            New blog post: #
            $maybe Project{projectHandle = project_handle} <- maybe_project
                <a href=@{BlogPostR project_handle blogPostHandle}>
                    #{blogPostTitle}

            $nothing
                #{blogPostTitle}
    |]

renderNewPledgeEvent :: SharesPledgedId -> SharesPledged -> UserMap -> Widget
renderNewPledgeEvent _ SharesPledged{..} user_map = do
    let pledger = lookupErr "renderNewPledgeEvent: pledger not found in user map" sharesPledgedUser user_map
    [whamlet|
        <div .event>
            ^{renderTime sharesPledgedTs}
            <a href=@{UserR sharesPledgedUser}> #{userDisplayName (Entity sharesPledgedUser pledger)}
            made new pledge of _{MsgShares sharesPledgedShares}!
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
            #{verb} their pledge from _{MsgShares old_shares} to _{MsgShares sharesPledgedShares}#{punc}
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
