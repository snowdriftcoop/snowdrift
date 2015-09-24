module Handler.Utils where

import Import

import qualified Data.Text as T

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
