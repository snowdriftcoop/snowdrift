{-# LANGUAGE CPP #-}

module Widgets.Time where

-- TODO make this prettier

import Import
import Data.Time

#if !(MIN_VERSION_time(1, 5, 0))
import System.Locale (defaultTimeLocale)
#endif

import Data.List

renderTime :: UTCTime -> Widget
renderTime time = do
    now <- liftIO getCurrentTime
    let render (UTCTime (ModifiedJulianDay 0) 0) = preEscapedToMarkup ("(soon)" :: Text);
        render t = preEscapedToMarkup . intercalate "&nbsp;" . words . formatTime defaultTimeLocale "%c" $ t

    toWidget [hamlet|
        <span title="#{render time}">
            #{showDiffTime now time}&nbsp;ago
    |]

