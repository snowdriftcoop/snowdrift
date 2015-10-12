module Model.Utils where

import Import

import Blaze.ByteString.Builder (Builder, toLazyByteString)
import Control.Monad.Reader (MonadReader, ask)
import Yesod (renderRoute)
import qualified Data.Map as M
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

routeToText :: MonadReader App m => Route App -> m Text
routeToText route = do
    app <- ask
    let (path_pieces, query_params) = renderRoute route
    return (b2t (joinPath app "" path_pieces query_params))
  where
    b2t :: Builder -> Text
    b2t = TL.toStrict . TLE.decodeUtf8 . toLazyByteString

-- This is not good, but its roots are deep.
lookupErr :: Ord k => String -> k -> Map k a -> a
lookupErr = M.findWithDefault . error
