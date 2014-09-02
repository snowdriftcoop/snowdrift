module Handler.Utils where

import Import

import           Blaze.ByteString.Builder (Builder, toLazyByteString)
import           Control.Monad.Reader     (MonadReader, ask)
import qualified Data.Text                as T
import qualified Data.Text.Lazy           as TL
import qualified Data.Text.Lazy.Encoding  as TLE
import           Yesod                    (renderRoute)

-- | Possible values for "mode" post param.
data PostMode
    = PostMode    -- Just "post"
    | PreviewMode -- Just "preview"
    | CancelMode  -- Just "cancel"

lookupPostMode :: Handler (Maybe PostMode)
lookupPostMode = lookupPostParam "mode" >>= \case
    Just "post"    -> return (Just PostMode)
    Just "preview" -> return (Just PreviewMode)
    Just "cancel"  -> return (Just CancelMode)
    _              -> return Nothing

lookupGetUTCTimeDefaultNow :: Text -> Handler UTCTime
lookupGetUTCTimeDefaultNow name = lookupGetParam name >>= \case
    Nothing    -> liftIO getCurrentTime
    Just value -> case reads (T.unpack value) of
        [(time,"")] -> return time
        _           -> liftIO getCurrentTime

routeToText :: MonadReader App m => Route App -> m Text
routeToText route = do
    app <- ask
    let (path_pieces, query_params) = renderRoute route
    return (b2t (joinPath app "" path_pieces query_params))
  where
    b2t :: Builder -> Text
    b2t = TL.toStrict . TLE.decodeUtf8 . toLazyByteString
