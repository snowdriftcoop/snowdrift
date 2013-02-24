module Model.User where


import Import

import qualified Data.Text as T

import Data.Maybe

data UserUpdate =
    UserUpdate
        { userUpdateName :: Maybe Text
        , userUpdateAvatar :: Maybe Text
        , userUpdateBlurb :: Maybe Markdown
        , userUpdateStatement :: Maybe Markdown
        }

updateUser :: forall (m :: * -> *).
                             PersistQuery m =>
                             Key (UserGeneric (PersistMonadBackend m)) -> UserUpdate -> m ()
updateUser user_id user_update = update user_id $ catMaybes
        [ (UserName =.) . Just <$> userUpdateName user_update
        , (UserStatement =.) . Just <$> userUpdateStatement user_update
        , (UserBlurb =.) . Just <$> userUpdateBlurb user_update
        , (UserAvatar =.) . Just <$> userUpdateAvatar user_update
        ]

applyUserUpdate :: User -> UserUpdate -> User
applyUserUpdate user user_update = user
        { userName = fromMaybe (userName user) $ Just <$> userUpdateName user_update
        , userStatement = fromMaybe (userStatement user) $ Just <$> userUpdateStatement user_update
        , userBlurb = fromMaybe (userBlurb user) $ Just <$> userUpdateBlurb user_update
        , userAvatar = fromMaybe (userAvatar user) $ Just <$> userUpdateAvatar user_update
        }


userPrintName :: Entity User -> Text
userPrintName (Entity user_id user) = fromMaybe (either (error . T.unpack) (T.append "user") $ fromPersistValue $ unKey user_id) (userName user)
