module Handler.Messages where

import Import

-- import Model.Role

import qualified Data.Map as M

import Widgets.Sidebar

getMessagesR :: Handler Html
getMessagesR = do
    Entity viewer_id viewer <- requireAuth
    now <- liftIO getCurrentTime

    let messages = []
    {- TODO
    messages <-
        runDB $ if userRole viewer == CommitteeMember || userRole viewer == Admin
         then selectList
            ( [ MessageTo ==. Just viewer_id ]
            ||. [ MessageTo ==. Nothing ]
            ) [ Desc MessageCreatedTs ]
         else selectList [ MessageTo ==. Just viewer_id ] [ Desc MessageCreatedTs ]
    -}

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

