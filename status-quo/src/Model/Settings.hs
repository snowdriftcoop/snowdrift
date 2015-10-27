
module Model.Settings where

import Import

import Data.Typeable

import Model.Settings.Internal

data UserSettings = UserSettings { userSettingsShowTagVotes :: Bool }
    deriving (Typeable)

defaultUserSettings :: UserSettings
defaultUserSettings = UserSettings { userSettingsShowTagVotes = False }

getUserSettings :: Handler UserSettings
getUserSettings = cached $ do
    maybe_user_id <- maybeAuthId
    case maybe_user_id of
        Nothing -> return defaultUserSettings
        Just user_id -> do
            ss <- runDB $ select $ from $ \user_setting -> do
                where_ $ user_setting ^. UserSettingUser ==. val user_id
                return user_setting

            return $
                foldl applyUserSetting defaultUserSettings $ map entityVal ss


applyUserSetting :: UserSettings -> UserSetting -> UserSettings
applyUserSetting
        user_settings
        setting@(UserSetting { userSettingSetting = ShowTagVotes }) =

    user_settings { userSettingsShowTagVotes = read (userSettingValue setting) }
