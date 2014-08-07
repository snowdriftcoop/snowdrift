module Handler.Messages where

import Import

import Model.User

import qualified Data.Map as M

import Model.Project

import Widgets.Time

getMessagesR :: Handler Html
getMessagesR = do
    viewer_entity@(Entity viewer_id viewer) <- requireAuth
    now <- liftIO getCurrentTime

    (messages, user_map) <- runDB $ do
        messages <- fetchUserMessagesDB viewer_id
        let from_users = catMaybes (map (messageFromUser . entityVal) messages)
        user_map <- entitiesMap . (viewer_entity :) <$> fetchUsersInDB from_users
        return (messages, user_map)

    let getUserName user_id =
            let user = user_map M.! user_id
             in fromMaybe (userIdent user) (userName user)

    _ <- runDB $ update $ \ user -> do
        set user [ UserReadMessages =. val now ]
        where_ ( user ^. UserId ==. val viewer_id )


    defaultLayout $ do
        setTitle "Messages | Snowdrift.coop"
        $(widgetFile "messages")

