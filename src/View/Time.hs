module View.Time (renderTime, iso8601) where

import Import

import Data.List
import Data.Time
import qualified Data.Text as T

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

showDiffTime :: UTCTime -> UTCTime -> String
showDiffTime x y =
  let secs_ago = round (diffUTCTime x y)
  in if | secs_ago < hour  -> go secs_ago minute "m"
        | secs_ago < day   -> go secs_ago hour   "h"
        | secs_ago < week  -> go secs_ago day    "d"
        | secs_ago < month -> go secs_ago week   "wk"
        | secs_ago < year  -> go secs_ago month  "mo"
        | otherwise        -> go secs_ago year   "yr"
  where
    go :: Integer -> Integer -> String -> String
    go secs_ago divisor suffix = show (secs_ago `div` divisor) ++ suffix

    minute = 60
    hour = minute * 60
    day = hour * 24
    week = day * 7
    month = day * 30
    year = day * 365
