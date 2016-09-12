module Avatar where

import Control.Lens
import Network.Libravatar

import Import.NoFoundation

getUserAvatar :: MonadHandler m => Route (HandlerSite m) -> Maybe User -> m Text
getUserAvatar defaultRoute muser = do
    defaultUrl <- getUrlRender <*> pure defaultRoute

    maybe (return defaultUrl)
          (\user ->
              liftIO (libravatar (user^.userEmail) defaultUrl))
          muser

  where
    libravatar :: Text -> Text -> IO Text
    libravatar e defUrl = do
        mavatar <-
            avatarUrl (Email e)
                      def
                          { optSecure = True
                          , optDefault = ImgCustom defUrl
                          , optSize = DefaultSize
                          , optTryGravatar = False
                          }
        return $ fromMaybe defUrl mavatar
