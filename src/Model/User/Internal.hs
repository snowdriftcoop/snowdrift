module Model.User.Internal where

import Import

data UserUpdate =
    UserUpdate
        { userUpdateName               :: Maybe Text
        , userUpdateAvatar             :: Maybe Text
        , userUpdateEmail              :: Maybe Text
        , userUpdateIrcNick            :: Maybe Text
        , userUpdateBlurb              :: Maybe Markdown
        , userUpdateStatement          :: Maybe Markdown
        }

data ChangePassphrase = ChangePassphrase
    { currentPassphrase :: Text
    , newPassphrase     :: Text
    , newPassphrase'    :: Text
    }

data SetPassphrase = SetPassphrase
    { passphrase  :: Text
    , passphrase' :: Text
    }


fetchUserEmail :: UserId -> DB (Maybe Text)
fetchUserEmail user_id
    = (\case []    -> Nothing
             (x:_) -> unValue x)
  <$> (select $ from $ \user -> do
           where_ $ user ^. UserId ==. val user_id
           return $ user ^. UserEmail)

fetchUserEmailVerified :: UserId -> DB (Maybe Text)
fetchUserEmailVerified user_id
    = (\case []    -> Nothing
             (x:_) -> unValue x)
  <$> (select $ from $ \user -> do
           where_ $ user ^. UserId ==. val user_id
                &&. user ^. UserEmail_verified
           return $ user ^. UserEmail)
