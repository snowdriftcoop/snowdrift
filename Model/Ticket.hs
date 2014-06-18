module Model.Ticket where

import Import

import Model.AnnotatedTag
import Model.Comment      (getCommentTags)

import qualified Data.Map as M
import qualified Data.Set as S

data AnnotatedTicket = AnnotatedTicket Text TicketId Ticket WikiPage Comment [AnnotatedTag]

getTickets :: ProjectId -> Text -> YesodDB App [AnnotatedTicket]
getTickets project_id project_handle = do
    tickets_info <- getTicketsInfo

    -- used_tags'tickets :: [(Set TagId, Map TagId Tag -> Handler AnnotatedTicket)]
    used_tags'tickets <-
        -- TODO: refactor this to avoid N+1 selects (for example, select all CommentTags where
        -- comment_id in comment_ids)
        forM tickets_info $ \(Entity ticket_id ticket, Entity comment_id comment, Entity _ page) -> do
            used_tags <- map entityVal <$> getCommentTags comment_id

            let t :: Map TagId Tag -> Handler AnnotatedTicket
                t tags_map = AnnotatedTicket project_handle ticket_id ticket page comment <$>
                    buildAnnotatedTags
                        tags_map
                        (CommentTagR project_handle (wikiPageTarget page) comment_id)
                        used_tags

            return (S.fromList $ map commentTagTag used_tags, t)

    tags_map <- M.fromList . map (entityKey &&& entityVal) <$>
        (select $
            from $ \tag -> do
            where_ $ tag ^. TagId `in_` valList (S.toList . mconcat $ map fst used_tags'tickets)
            return tag)

    mapM (\(_, t) -> lift $ t tags_map) used_tags'tickets
  where
    -- Get all of this Project's Ticket/Comment/WikiPage tuples, for all open tickets.
    getTicketsInfo :: YesodDB App [(Entity Ticket, Entity Comment, Entity WikiPage)]
    getTicketsInfo =
        select $
            from $ \(ticket `InnerJoin` comment `InnerJoin` page) -> do
            on_ (page ^. WikiPageDiscussion ==. comment ^. CommentDiscussion)
            on_ (comment ^. CommentId ==. ticket ^. TicketComment)
            where_ (page ^. WikiPageProject ==. val project_id &&.
                    comment ^. CommentId `notIn` subList_select
                                                     (from $ \closure ->
                                                      return $ closure ^. CommentClosureComment))
            return (ticket, comment, page)
