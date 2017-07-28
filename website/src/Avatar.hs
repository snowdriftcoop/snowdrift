module Avatar where

import Control.Exception (PatternMatchFail)
import Control.Lens
import qualified Data.Text as T
import Network.Libravatar

import Import.NoFoundation

getUserAvatar :: MonadHandler m => Route (HandlerSite m) -> Maybe User -> m Text
getUserAvatar defaultRoute muser = do
    root <- pure "https://snowdrift.coop"
    defaultUrl <- getUrlRender <*> pure defaultRoute
    let defaultUrlText = T.append root defaultUrl

    maybe (return defaultUrl)
          (\user ->
              liftIO (libravatar (user^.userEmail) defaultUrlText))
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
            -- When developing with no internet connection, this exception
            -- happens.
            (\(_ :: PatternMatchFail) -> pure Nothing))
