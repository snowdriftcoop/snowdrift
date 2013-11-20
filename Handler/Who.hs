module Handler.Who where

import Import

import Widgets.Sidebar

import Data.List (sortBy)

import Model.Markdown

userShortName :: User -> Text
userShortName user = fromMaybe (userIdent user) $ userName user

getWhoR :: Text -> Handler Html
getWhoR project_handle = do
    committee_members <- runDB $ do
        Entity project_id _ <- getBy404 $ UniqueProjectHandle project_handle

 	select $ from $ \ (user `InnerJoin` committee_user) -> do
                on_ $ user ^. UserId ==. committee_user ^. CommitteeUserUser
                where_ $ committee_user ^. CommitteeUserProject ==. val project_id
                return (user, committee_user)

    let sorted = sortBy (compare `on` (committeeUserCreatedTs . entityVal . snd)) committee_members
        members :: [Entity User] = map fst sorted

    defaultLayout $(widgetFile "who")

