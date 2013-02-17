module Handler.Tickets where

import Import

import Widgets.Sidebar

import qualified Data.Map as M
import Control.Arrow ((&&&))

getTicketsR :: Handler RepHtml
getTicketsR = do
    (tickets, pages_by_ticket_id) <- runDB $ do
        unfiltered_tickets <- selectList [] [Desc TicketId]
        comments <- selectList [ CommentId <-. map (ticketComment . entityVal) unfiltered_tickets ] []
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
        
