module Handler.Who where

import Import

import Widgets.Sidebar

import Data.List (sortBy)

import Database.Persist.Query.Join (selectOneMany)
import Database.Persist.Query.Join.Sql (runJoin)

import Data.Function (on)

import qualified Data.Text as T

userShortName :: User -> Text
userShortName user = fromMaybe (userIdent user) $ do
    name <- userName user
    listToMaybe $ T.words name

getWhoR :: Handler RepHtml
getWhoR = do
    committee_members :: [(Entity User, [Entity CommitteeUser])] <- runDB $ runJoin $ selectOneMany (CommitteeUserUser <-.) committeeUserUser
    let sorted = sortBy (compare `on` (map (committeeUserCreatedTs . entityVal) . snd)) committee_members
        members :: [Entity User] = map fst sorted
    defaultLayout $(widgetFile "who")

