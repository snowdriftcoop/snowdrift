module Handler.Messages where

import Import

import Model.Role

import Control.Arrow

import qualified Data.Map as M

import Widgets.Sidebar

getMessagesR :: Handler RepHtml
getMessagesR = do
    Entity viewer_id viewer <- requireAuth
    now <- liftIO getCurrentTime

    messages <-
        runDB $ if userRole viewer == CommitteeMember || userRole viewer == Admin
         then selectList
            ( [ MessageTo ==. Just viewer_id ]
            ||. [ MessageTo ==. Nothing ]
            ) [ Desc MessageCreatedTs ]
         else selectList [ MessageTo ==. Just viewer_id ] [ Desc MessageCreatedTs ]

    users <- runDB $ selectList [ UserId <-. mapMaybe (messageFrom . entityVal) messages ] []
    let user_map = M.fromList $ map (entityKey &&& entityVal) users
        getUserName user_id =
            let user = user_map M.! user_id
             in fromMaybe (userIdent user) (userName user)

    _ <- runDB $ update viewer_id [ UserReadMessages =. now ]


    defaultLayout $(widgetFile "messages")
