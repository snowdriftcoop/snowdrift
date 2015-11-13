module Handler.SnowdriftEvent where

import Import

import Control.Applicative

import Handler.Comment
import Model.Project
import WrappedValues

redirectCommentEvent :: (a -> YDB b) -> (b -> CommentId) -> a -> HandlerT App IO ()
redirectCommentEvent get_object get_comment_from_object =
        get_object
    >>> runYDB
    >>> fmap get_comment_from_object
    >=> getCommentDirectLinkR


getEventCommentPostedR :: EventCommentPostedId -> Handler ()
getEventCommentPostedR = redirectCommentEvent get404 eventCommentPostedComment

getEventCommentPendingR :: EventCommentPendingId -> Handler ()
getEventCommentPendingR = redirectCommentEvent get404 eventCommentPendingComment

getEventCommentRethreadedR :: EventCommentRethreadedId -> Handler ()
getEventCommentRethreadedR = redirectCommentEvent (get404 >=> eventCommentRethreadedRethread >>> get404) rethreadNewComment

getEventCommentClosingR :: EventCommentClosingId -> Handler ()
getEventCommentClosingR = redirectCommentEvent (get404 >=> eventCommentClosingCommentClosing >>> get404) commentClosingComment

getEventTicketClaimedR :: EventTicketClaimedId -> Handler ()
getEventTicketClaimedR = redirectCommentEvent
    (       get404
        >=> (eventTicketClaimedClaim &&& eventTicketClaimedOldClaim)
        >>> (fmap Left *** fmap Right)
        >>> uncurry (<|>)
        >>> maybe (lift notFound) (either (fmap Left . get404) (fmap Right . get404))
    )
    (either ticketClaimingTicket ticketOldClaimingTicket)

getEventTicketUnclaimedR :: EventTicketUnclaimedId -> Handler ()
getEventTicketUnclaimedR = redirectCommentEvent (get404 >=> eventTicketUnclaimedClaim >>> get404) ticketOldClaimingTicket

getEventWikiPageR :: EventWikiPageId -> Handler ()
getEventWikiPageR event_wiki_page_id = do
    languages <- getLanguages

    runYDB $ do
        EventWikiPage{..}   <- get404 event_wiki_page_id
        WikiPage{..}        <- get404 eventWikiPageWikiPage
        Project{..}         <- get404 wikiPageProject

        targets <- select $ from $ \wt -> do
            where_ $ wt ^. WikiTargetPage ==. val eventWikiPageWikiPage
            return wt

        [ wiki_edit_id ] <- fmap unwrapValues $ select $ from $ \we -> do
            where_ $ we ^. WikiEditPage ==. val eventWikiPageWikiPage
            orderBy [ asc $ we ^. WikiEditTs ]
            limit 1
            return $ we ^. WikiEditId

        let (Entity _ WikiTarget{..}:_) = pickTargetsByLanguage languages targets

        lift $ redirect $ WikiEditR projectHandle wikiTargetLanguage wikiTargetTarget wiki_edit_id

getEventWikiEditR :: EventWikiEditId -> Handler ()
getEventWikiEditR event_wiki_edit_id = undefined event_wiki_edit_id

getEventNewPledgeR :: EventNewPledgeId -> Handler ()
getEventNewPledgeR event_new_pledge_id = undefined event_new_pledge_id

getEventUpdatedPledgeR :: EventUpdatedPledgeId -> Handler ()
getEventUpdatedPledgeR event_updated_pledge_id = undefined event_updated_pledge_id

getEventDeletedPledgeR :: EventDeletedPledgeId -> Handler ()
getEventDeletedPledgeR event_deleted_pledge_id = undefined event_deleted_pledge_id

getEventBlogPostR :: EventBlogPostId -> Handler ()
getEventBlogPostR event_blog_post_id = undefined event_blog_post_id

