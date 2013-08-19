module Handler.Who where

import Import

import Widgets.Sidebar

import Data.List (sortBy)

import qualified Data.Function as FUN

import Model.Markdown

userShortName :: User -> Text
userShortName user = fromMaybe (userIdent user) $ userName user

getWhoR :: Text -> Handler Html
getWhoR project_handle = do
    committee_members <- runDB $ select $ from $ \ (user `InnerJoin` committee_user) -> do
        on (user ^. UserId ==. committee_user ^. CommitteeUserUser)
        return (user, committee_user)

    let sorted = sortBy (compare `FUN.on` (committeeUserCreatedTs . entityVal . snd)) committee_members
        members :: [Entity User] = map fst sorted

    defaultLayout $(widgetFile "who")

