module Model.User where

import Import

import qualified Data.Map  as M
import qualified Data.Text as T

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

getUsersIn :: [UserId] -> YesodDB App [Entity User]
getUsersIn user_ids = selectList [UserId <-. user_ids] []

updateUser :: UserId -> UserUpdate -> YesodDB App ()
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

userWidget :: UserId -> Widget
userWidget user_id = do
    maybe_user <- handlerToWidget $ runDB $ get user_id
    case maybe_user of
        Nothing -> [whamlet|deleted user|]
        Just user ->
            [whamlet|
                <a href=@{UserR user_id}>
                    #{userPrintName (Entity user_id user)}
            |]

isEstablished :: User -> Bool
isEstablished = isJust . userEstablishedTs

{- isProject___ stuff below is almost all redundant. It really should be
- refactored into being a main function to lookup affiliations and tiny
- functions for each affiliation -}

isProjectAdmin :: Text -> UserId -> YesodDB App Bool
isProjectAdmin project_handle user_id =
    fmap (not . null) $ select $ from $ \ (pur `InnerJoin` p) -> do
        on_ $ pur ^. ProjectUserRoleProject ==. p ^. ProjectId
        where_ $ p ^. ProjectHandle ==. val project_handle
            &&. pur ^. ProjectUserRoleUser ==. val user_id
            &&. pur ^. ProjectUserRoleRole ==. val Admin
        limit 1
        return ()

isProjectTeamMember :: Text -> UserId -> YesodDB App Bool
isProjectTeamMember project_handle user_id =
    fmap (not . null) $ select $ from $ \ (pur `InnerJoin` p) -> do
        on_ $ pur ^. ProjectUserRoleProject ==. p ^. ProjectId
        where_ $ p ^. ProjectHandle ==. val project_handle
            &&. pur ^. ProjectUserRoleUser ==. val user_id
            &&. pur ^. ProjectUserRoleRole ==. val TeamMember
        limit 1
        return ()

isProjectModerator :: Text -> UserId -> YesodDB App Bool
isProjectModerator project_handle user_id =
    fmap (not . null) $ select $ from $ \ (pur `InnerJoin` p) -> do
        on_ $ pur ^. ProjectUserRoleProject ==. p ^. ProjectId
        where_ $ p ^. ProjectHandle ==. val project_handle
            &&. pur ^. ProjectUserRoleUser ==. val user_id
            &&. pur ^. ProjectUserRoleRole ==. val Moderator
        limit 1
        return ()

isCurUserProjectModerator :: Text -> Handler Bool
isCurUserProjectModerator project_handle =
    maybeAuthId >>= maybe (return False) (runDB . isProjectModerator project_handle)

isProjectAffiliated :: Text -> UserId -> YesodDB App Bool
isProjectAffiliated project_handle user_id =
    fmap (not . null) $ select $ from $ \ (pur `InnerJoin` p) -> do
        on_ $ pur ^. ProjectUserRoleProject ==. p ^. ProjectId
        where_ $ p ^. ProjectHandle ==. val project_handle
            &&. pur ^. ProjectUserRoleUser ==. val user_id
        limit 1
        return ()

