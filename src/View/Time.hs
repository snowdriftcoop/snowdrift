module View.Time where

import Import

import Data.List
import qualified Data.Text as T
import Data.Time

iso8601 :: UTCTime -> T.Text
iso8601 t = T.pack $ formatTime defaultTimeLocale "%FT%T%z" t

renderTime :: UTCTime -> Widget
renderTime time = do
    now <- liftIO getCurrentTime
    let render (UTCTime (ModifiedJulianDay 0) 0) = preEscapedToMarkup ("(soon)" :: Text);
        render t = preEscapedToMarkup . intercalate "&nbsp;" . words . formatTime defaultTimeLocale "%c" $ t

    toWidget [hamlet|
        <span title=#{render time}>
            #{showDiffTime now time}&nbsp;ago
    |]
