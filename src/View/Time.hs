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
  in if | secs_ago < secsPerHour  -> go secs_ago secsPerMinute "m"
        | secs_ago < secsPerDay   -> go secs_ago secsPerHour   "h"
        | secs_ago < secsPerWeek  -> go secs_ago secsPerDay    "d"
        | secs_ago < secsPerMonth -> go secs_ago secsPerWeek   "wk"
        | secs_ago < secsPerYear  -> go secs_ago secsPerMonth  "mo"
        | otherwise               -> go secs_ago secsPerYear   "yr"
  where
    go secs_ago divisor suffix = show (secs_ago `div` divisor) ++ suffix

    secsPerMinute,
        secsPerHour,
        secsPerDay,
        secsPerWeek,
        secsPerMonth,
        secsPerYear
        :: Integer
    secsPerMinute = 60
    secsPerHour   = 60*60
    secsPerDay    = 60*24
    secsPerWeek   = 60*60*24*7
    secsPerMonth  = 60*60*24*30
    secsPerYear   = 60*60*24*365
