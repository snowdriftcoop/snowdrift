module Handler.Messages where

import Import

-- import Model.Role

import Control.Arrow

import qualified Data.Map as M

import Widgets.Sidebar

getMessagesR :: Handler Html
getMessagesR = do
    Entity viewer_id viewer <- requireAuth
    now <- liftIO getCurrentTime

    let messages = []
    {- TODO
    messages <-
        runDB $ let view_all = if userRole viewer == CommitteeMember || userRole viewer == Admin
                        then (\ message -> (||. message ^. MessageTo ==. val Nothing))
                        else const id
                 in select $ from $ \ message -> do
                        where_ $ view_all message ( message ^. MessageTo ==. val (Just viewer_id) )
                        orderBy [ desc (message ^. MessageCreatedTs) ]
                        return message
    -}

    users <- runDB $ select $ from $ \ user -> do
        where_ (user ^. UserId `in_` valList (mapMaybe (messageFrom . entityVal) messages))
        return user

    let user_map = M.fromList $ map (entityKey &&& entityVal) users
        getUserName user_id =
            let user = user_map M.! user_id
             in fromMaybe (userIdent user) (userName user)

    _ <- runDB $ update $ \ user -> do
        set user [ UserReadMessages =. val now ]
        where_ ( user ^. UserId ==. val viewer_id )


    defaultLayout $(widgetFile "messages")
