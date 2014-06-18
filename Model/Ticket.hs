module Model.Ticket where

import Import

import Model.AnnotatedTag
import Model.Comment      (getCommentTags)

import qualified Data.Map as M
import qualified Data.Set as S

data AnnotatedTicket = AnnotatedTicket Text TicketId Ticket WikiPage Comment [AnnotatedTag]

getTickets :: Text -> YesodDB App [AnnotatedTicket]
getTickets project_handle = do
    tickets_info <- getTicketsInfo

    used_tags'tickets <- forM tickets_info $ \(Entity ticket_id ticket, Entity comment_id comment, Entity _ page) -> do
        used_tags <- getCommentTags comment_id

        let tags' :: [(TagId, (UserId, Int))]
            tags' = map ((commentTagTag &&& (commentTagUser &&& commentTagCount)) . entityVal) used_tags

            t :: Map TagId Tag -> Handler AnnotatedTicket
            t tags_map = AnnotatedTicket project_handle ticket_id ticket page comment <$>
                buildAnnotatedTags tags_map (CommentTagR project_handle (wikiPageTarget page) comment_id) tags'

        return (S.fromList $ map (commentTagTag . entityVal) used_tags, t)

    tags <-
        select $
            from $ \tag -> do
            where_ $ tag ^. TagId `in_` valList (S.toList . mconcat $ map fst used_tags'tickets)
            return tag

    let tags_map = M.fromList . map (entityKey &&& entityVal) $ tags

    mapM (\(_, t) -> lift $ t tags_map) used_tags'tickets
  where
    -- Get all Ticket/Comment/WikiPage tuples, for all tickets that aren't closed.
    getTicketsInfo :: YesodDB App [(Entity Ticket, Entity Comment, Entity WikiPage)]
    getTicketsInfo =
        select $
            from $ \(ticket `InnerJoin` comment `InnerJoin` page) -> do
            on_ $ page ^. WikiPageDiscussion ==. comment ^. CommentDiscussion
            on_ $ comment ^. CommentId ==. ticket ^. TicketComment
            where_ $ comment ^. CommentId `notIn` subList_select
                                                      (from $ \closure ->
                                                       return $ closure ^. CommentClosureComment)
            return (ticket, comment, page)
