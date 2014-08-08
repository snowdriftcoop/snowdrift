module Handler.Messages where

import Import

import           Model.Message
import           Model.Project
import           Model.User
import           Widgets.Time

import           Data.Maybe
import qualified Data.Map   as M

getMessagesR :: Handler Html
getMessagesR = do
    user@(Entity user_id _) <- requireAuth

    (messages, user_map) <- runDB $ do
        messages <- fetchUserMessagesDB user_id
        let from_users = catMaybes (map (messageFromUser . entityVal) messages)
        user_map <- entitiesMap . (user :) <$> fetchUsersInDB from_users
        userReadMessagesDB user_id
        return (messages, user_map)

    defaultLayout $ do
        setTitle "Messages | Snowdrift.coop"
        $(widgetFile "messages")

postArchiveMessageR :: MessageId -> Handler ()
postArchiveMessageR message_id = do
    user_id <- requireAuthId
    runYDB $ do
        message <- get404 message_id
        unless (user_id == messageToUser message) $
            lift (permissionDenied "You can't archive this message.")
        archiveMessageDB message_id
