module Model.Utils where

import Import

import           Blaze.ByteString.Builder (Builder, toLazyByteString)
import           Control.Monad.Reader     (MonadReader, ask)
import qualified Data.Text.Lazy           as TL
import qualified Data.Text.Lazy.Encoding  as TLE
import           Yesod                    (renderRoute)

routeToText :: MonadReader App m => Route App -> m Text
routeToText route = do
    app <- ask
    let (path_pieces, query_params) = renderRoute route
    return (b2t (joinPath app "" path_pieces query_params))
  where
    b2t :: Builder -> Text
    b2t = TL.toStrict . TLE.decodeUtf8 . toLazyByteString
