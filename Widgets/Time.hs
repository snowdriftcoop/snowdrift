
module Widgets.Time where

-- TODO make this prettier

import Import

import Data.Time.Format
import System.Locale

import Data.List

import Text.Blaze

renderTime :: UTCTime -> Widget
renderTime time = do
    now <- liftIO getCurrentTime
    let render = preEscapedToMarkup . intercalate "&nbsp;" . words . formatTime defaultTimeLocale "%c %Z"
    toWidget [hamlet|
        <span title="#{age now time} ago">
            #{render time}
    |]
