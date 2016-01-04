module Data.Avatar where

import qualified Data.Text as T
import Network.Libravatar

import Import.NoFoundation

getUserAvatar :: Text -> Maybe User -> IO Text
getUserAvatar defaultUrl muser = maybe
        (return defaultUrl)
        (\user -> do
            let email = fromMaybe T.empty (userEmail user)
            liftIO $ if (validEmail user email)
                     then (libravatar email defaultUrl)
                     else return defaultUrl)
        muser
    where
        validEmail :: User -> Text -> Bool
        validEmail u e = (not (T.null e)) && (userEmail_verified u)

        libravatar :: Text -> Text -> IO Text
        libravatar e defaultUrl = do
                mavatar <- avatarUrl
                         (Email $ T.unpack e)
                         defOpts
                             { optSecure = False
                             , optDefault = ImgCustom (T.unpack defaultUrl)
                             , optSize = DefaultSize
                             , optTryGravatar = False
                             }
                return $ maybe defaultUrl T.pack mavatar

