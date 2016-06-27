module Avatar where

import Control.Lens
import Network.Libravatar
import qualified Data.Text as T

import Import.NoFoundation

getUserAvatar :: MonadHandler m => Route (HandlerSite m) -> Maybe User -> m Text
getUserAvatar defaultRoute muser = do
    defaultUrl <- getUrlRender <*> pure defaultRoute

    maybe (return defaultUrl)
          (\user -> do
              liftIO (libravatar (user^.userEmail) defaultUrl))
          muser

  where
    libravatar :: Text -> Text -> IO Text
    libravatar e defUrl = do
        mavatar <-
            avatarUrl (Email $ T.unpack e)
                      defOpts
                          { optSecure = True
                          , optDefault = ImgCustom (T.unpack defUrl)
                          , optSize = DefaultSize
                          , optTryGravatar = False
                          }
        return $ maybe defUrl T.pack mavatar
