module Model.User where


import Import

import qualified Data.Text as T

data UserUpdate =
    UserUpdate
        { userUpdateName :: Maybe Text
        , userUpdateAvatar :: Maybe Text
        , userUpdateBlurb :: Maybe Markdown
        , userUpdateStatement :: Maybe Markdown
        }

updateUser :: PersistQuery backend m => Key backend (UserGeneric backend1) -> UserUpdate -> backend m ()
updateUser user_id user_update = update user_id $
        (maybe [] (return . (UserName =.) . Just) (userUpdateName user_update))
        ++ (maybe [] (return . (UserStatement =.) . Just) (userUpdateStatement user_update))
        ++ (maybe [] (return . (UserBlurb =.) . Just) (userUpdateBlurb user_update))
        ++ (maybe [] (return . (UserAvatar =.) . Just) (userUpdateAvatar user_update))


userPrintName :: Entity User -> Text
userPrintName (Entity user_id user) = fromMaybe (either (error . T.unpack) (T.append "user") $ fromPersistValue $ unKey user_id) (userName user)
