module Handler.Who where

import Import



import Data.List (sortBy)

import Model.Markdown

userShortName :: User -> Text
userShortName user = fromMaybe (userIdent user) $ userName user

getWhoR :: Text -> Handler Html
getWhoR project_handle = do
    Entity project_id project <- runDB $ getBy404 $ UniqueProjectHandle project_handle
    committee_members <- runDB $ do
        select $ from $ \ (user `InnerJoin` committee_user) -> do
                on_ $ user ^. UserId ==. committee_user ^. CommitteeUserUser
                where_ $ committee_user ^. CommitteeUserProject ==. val project_id
                return (user, committee_user)

    let sorted = sortBy (compare `on` (committeeUserCreatedTs . entityVal . snd)) committee_members
        members :: [Entity User] = map fst sorted

    defaultLayout $ do
        setTitle . toHtml $ projectName project <> " - Committee | Snowdrift.coop"
        $(widgetFile "who")

