-- | Utilities commonly useful to all Handlers.
module Handler.Utils where

import Import

import Data.List (sortBy, (\\))
import Data.Text.Titlecase
import Network.Mail.Mime (randomString)
import System.Random (newStdGen)
import Yesod.Core
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

snowdriftTitle :: MonadWidget m => Text -> m ()
snowdriftTitle t = setTitle $
    (toHtml $ titlecase $ T.toLower $ t) <>
    (toHtml (" | Snowdrift.coop" :: Text))

snowdriftDashTitle :: MonadWidget m => Text -> Text -> m ()
snowdriftDashTitle x y = snowdriftTitle $ x <> " — " <> y

newHash :: IO Text
newHash = T.pack . fst . randomString 42 <$> newStdGen

makeLanguageOptions :: Handler (OptionList Language)
makeLanguageOptions = do
    preferred_languages <- getLanguages

    app <- getYesod

    langs <- languages

    let render :: Language -> Text
        render = renderMessage app langs . MsgLangName

    return $ OptionList
        { olOptions = map (mkOption render) $ preferred_languages ++ (sortBy (compare `on` render) $ [minBound..maxBound] \\ preferred_languages)
        , olReadExternal = fromPathPiece
        }
  where
    mkOption render language = Option
            { optionDisplay = render language
            , optionInternalValue = language
            , optionExternalValue = toPathPiece language
            }
