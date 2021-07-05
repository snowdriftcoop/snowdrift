{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Crowdmatch (crowdmatch)
import RunPersist (runPersistKeter)

import Data.Time (Day, fromGregorian, getCurrentTime, utctDay)
import System.Environment (getArgs)
import System.Exit (ExitCode(ExitFailure), exitWith)
import Text.Read (readMaybe)

usage :: IO a -- exits
usage = do
  putStrLn "usage: crowdmatch [YEAR MONTH DAY]"
  putStrLn "e.g. crowdmatch 2019 09 01"
  exitWith (ExitFailure 2)

-- In the future we should use ISO 8601 format dates. Newer versions of the
-- `time` library are able to parse them, so we can replace parseDate with
-- import Data.Time.Format.ISO8601 (iso8601ParseM)
-- However, the `time` library is a common dependency so we can't just bump
-- its version; we need to update all of our deps.. which we ought to do anyway,
-- but will take more time (heh) than I want to spend right now.
parseDate :: String -> String -> String -> Maybe Day
parseDate yyyy mm dd = do
  year <- readMaybe yyyy
  month <- readMaybe mm
  day <- readMaybe dd
  return (fromGregorian year month day)

-- NB! The string passed to runPersistKeter must match the APPNAME used in
-- keter.sh to deploy the app. Must fix.
runCrowdmatch :: Day -> IO ()
runCrowdmatch day = runPersistKeter "SnowdriftReboot" (crowdmatch day)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      day <- utctDay <$> getCurrentTime
      runCrowdmatch day
    yyyy : mm : dd : [] -> maybe usage runCrowdmatch (parseDate yyyy mm dd)
    _ -> usage
