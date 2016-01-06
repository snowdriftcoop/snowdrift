module Data.Avatar where

import qualified Data.Text as T
import Network.Libravatar

import Import.NoFoundation

getUserAvatar :: MonadHandler m => Route (HandlerSite m) -> Maybe User -> m Text
getUserAvatar defaultRoute muser = do
    render <- getUrlRender
    let defaultUrl = render defaultRoute

    av <- maybe
                (return defaultUrl)
                (\user -> do
                    let email = fromMaybe T.empty (userEmail user)
                    liftIO $ if validEmail user email
                                 then libravatar email defaultUrl
                                 else return defaultUrl)
                muser
    return av

    where
        validEmail :: User -> Text -> Bool
        validEmail u e = not (T.null e) && userEmail_verified u

        libravatar :: Text -> Text -> IO Text
        libravatar e defUrl = do
                mavatar <- avatarUrl
                         (Email $ T.unpack e)
                        defOpts
                             { optSecure = False
                             , optDefault = ImgCustom (T.unpack defUrl)
                             , optSize = DefaultSize
                             , optTryGravatar = False
                             }
                return $ maybe defUrl T.pack mavatar
