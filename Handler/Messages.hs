module Handler.Messages where

import Import

import Model.User

import qualified Data.Map as M

import Widgets.Sidebar
import Widgets.Time

getMessagesR :: Handler Html
getMessagesR = do
    Entity viewer_id viewer <- requireAuth
    now <- liftIO getCurrentTime

    messages <-
        -- TODO: filter by projects?
        runDB $ do
            snowdrift_member <- isProjectAffiliated "snowdrift" viewer_id
            select $ from $ \ message -> do
                where_ $ if snowdrift_member
                    then message ^. MessageTo ==. val (Just viewer_id) ||. isNothing (message ^. MessageTo)
                    else message ^. MessageTo ==. val (Just viewer_id)
                orderBy [ desc $ message ^. MessageCreatedTs ]
                return message

    users <- runDB $ select $ from $ \ user -> do
        where_ (user ^. UserId `in_` valList (mapMaybe (messageFrom . entityVal) messages))
        return user

    let user_map = M.fromList $ ((viewer_id, viewer):) $ map (entityKey &&& entityVal) users
        getUserName user_id =
            let user = user_map M.! user_id
             in fromMaybe (userIdent user) (userName user)

    _ <- runDB $ update $ \ user -> do
        set user [ UserReadMessages =. val now ]
        where_ ( user ^. UserId ==. val viewer_id )


    defaultLayout $(widgetFile "messages")

