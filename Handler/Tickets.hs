module Handler.Tickets where

import Import

import Widgets.Sidebar

import qualified Data.Map as M
import Control.Arrow ((&&&))

getOldTicketsR :: Handler RepHtml
getOldTicketsR = getTicketsR "old_snowdrift"

getTicketsR :: Text -> Handler RepHtml
getTicketsR project_handle = do
    (tickets, pages_by_ticket_id) <- runDB $ do
        Entity project_id _ <- getBy404 $ UniqueProjectHandle project_handle
        unfiltered_tickets <- selectList [] [Desc TicketId]
        comments <- selectList [ CommentId <-. map (ticketComment . entityVal) unfiltered_tickets, CommentProject ==. project_id ] []
        retractions <- map (commentRetractionComment . entityVal) <$> selectList [ CommentRetractionComment <-. map entityKey comments ] []

        pages <- selectList [ WikiPageId <-. map (commentPage . entityVal) comments ] []

        let tickets = filter ((`notElem` retractions) . ticketComment . entityVal) unfiltered_tickets
            tickets_map = M.fromList . map (entityKey &&& entityVal) $ tickets
            comments_map = M.fromList . map (entityKey &&& entityVal) $ comments
            pages_map = M.fromList . map (entityKey &&& entityVal) $ pages

            comments_by_ticket_id = M.mapMaybe (flip M.lookup comments_map . ticketComment) tickets_map
            pages_by_ticket_id = M.mapMaybe (flip M.lookup pages_map . commentPage) comments_by_ticket_id
        return (tickets, pages_by_ticket_id)

    defaultLayout $ $(widgetFile "tickets")
        
