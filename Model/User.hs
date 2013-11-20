module Model.User where


import Import

import qualified Data.Text as T

import Data.Maybe

import Control.Monad.Trans.Resource

import Model.Role

data UserUpdate =
    UserUpdate
        { userUpdateName :: Maybe Text
        , userUpdateAvatar :: Maybe Text
        , userUpdateIrcNick :: Maybe Text
        , userUpdateBlurb :: Maybe Markdown
        , userUpdateStatement :: Maybe Markdown
        }

updateUser :: (MonadLogger m, MonadIO m, MonadBaseControl IO m, MonadThrow m, MonadUnsafeIO m) => Key User -> UserUpdate -> SqlPersistT m ()
updateUser user_id user_update = update $ \ user -> do
        set user $ catMaybes
            [ (UserName =.) . val . Just <$> userUpdateName user_update
            , (UserAvatar =.) . val . Just <$> userUpdateAvatar user_update
            , (UserIrcNick =.) . val . Just <$> userUpdateIrcNick user_update
            , (UserStatement =.) . val . Just <$> userUpdateStatement user_update
            , (UserBlurb =.) . val . Just <$> userUpdateBlurb user_update
            ]
        where_ ( user ^. UserId ==. val user_id )

applyUserUpdate :: User -> UserUpdate -> User
applyUserUpdate user user_update = user
        { userName = fromMaybe (userName user) $ Just <$> userUpdateName user_update
        , userAvatar = fromMaybe (userAvatar user) $ Just <$> userUpdateAvatar user_update
        , userIrcNick = fromMaybe (userIrcNick user) $ Just <$> userUpdateIrcNick user_update
        , userStatement = fromMaybe (userStatement user) $ Just <$> userUpdateStatement user_update
        , userBlurb = fromMaybe (userBlurb user) $ Just <$> userUpdateBlurb user_update
        }


userPrintName :: Entity User -> Text
userPrintName (Entity user_id user) = fromMaybe (either (error . T.unpack) (T.append "user") $ fromPersistValue $ unKey user_id) (userName user)

isProjectAdmin :: (MonadIO m, MonadLogger m, MonadBaseControl IO m, MonadUnsafeIO m, MonadThrow m)
    => Text -> UserId -> SqlPersistT m Bool
isProjectAdmin project_handle user_id = fmap (not . null) $ select $ from $ \ (pur `InnerJoin` p) -> do
    on_ $ pur ^. ProjectUserRoleProject ==. p ^. ProjectId
    where_ $ p ^. ProjectHandle ==. val project_handle
        &&. pur ^. ProjectUserRoleUser ==. val user_id
        &&. pur ^. ProjectUserRoleRole ==. val Admin
    limit 1
    return ()
    
isProjectAffiliated :: (MonadIO m, MonadLogger m, MonadBaseControl IO m, MonadUnsafeIO m, MonadThrow m)
    => Text -> UserId -> SqlPersistT m Bool
isProjectAffiliated project_handle user_id = fmap (not . null) $ select $ from $ \ (pur `InnerJoin` p) -> do
    on_ $ pur ^. ProjectUserRoleProject ==. p ^. ProjectId
    where_ $ p ^. ProjectHandle ==. val project_handle
        &&. pur ^. ProjectUserRoleUser ==. val user_id
    limit 1
    return ()
