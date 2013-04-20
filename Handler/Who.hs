module Handler.Who where

import Import

import Widgets.Sidebar

import Data.List (sortBy)

import Database.Persist.Query.Join (selectOneMany)
import Database.Persist.Query.Join.Sql (runJoin)

import Data.Function (on)

userShortName :: User -> Text
userShortName user = fromMaybe (userIdent user) $ userName user

getWhoR :: Handler RepHtml
getWhoR = do
    committee_members :: [(Entity User, [Entity CommitteeUser])] <- runDB $ runJoin $ selectOneMany (CommitteeUserUser <-.) committeeUserUser
    let sorted = sortBy (compare `on` (map (committeeUserCreatedTs . entityVal) . snd)) committee_members
        members :: [Entity User] = map fst sorted
    defaultLayout $(widgetFile "who")

