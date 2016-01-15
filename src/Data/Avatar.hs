module Data.Avatar where

import qualified Data.Text as T
import Network.Libravatar

import Import.NoFoundation

getUserAvatar :: MonadHandler m => Route (HandlerSite m) -> Maybe User -> m Text
getUserAvatar defaultRoute muser = do
    defaultUrl <- getUrlRender <*> pure defaultRoute

    maybe (return defaultUrl)
          (\user -> do
              let email = fromMaybe T.empty (userEmail user)
              if userEmail_verified user
                  then liftIO (libravatar email defaultUrl)
                  else return defaultUrl)
          muser

  where
    libravatar :: Text -> Text -> IO Text
    libravatar e defUrl = do
        mavatar <-
            avatarUrl (Email $ T.unpack e)
                      defOpts
                          { optSecure = False
                          , optDefault = ImgCustom (T.unpack defUrl)
                          , optSize = DefaultSize
                          , optTryGravatar = False
                          }
        return $ maybe defUrl T.pack mavatar
