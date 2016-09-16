{-# LANGUAGE ScopedTypeVariables #-}
module Avatar where

import Control.Exception (PatternMatchFail)
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
    libravatar e defUrl = fmap (fromMaybe defUrl)
        (catch
            (avatarUrl
                (Email e)
                def { optSecure = True
                    , optDefault = ImgCustom defUrl
                    , optSize = DefaultSize
                    , optTryGravatar = False
                    })
            -- | When developing with no internet connection, this exception
            -- happens.
            (\(_ :: PatternMatchFail) -> pure Nothing))
