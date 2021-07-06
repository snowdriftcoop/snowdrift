{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Crowdmatch (crowdmatch)
import RunPersist (runPersistKeter)

import Data.List (partition)
import Data.Time (Day, fromGregorian, getCurrentTime, utctDay)
import System.Environment (getArgs)
import System.Exit (ExitCode(ExitFailure), exitWith)
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

usage :: IO a -- exits
usage = do
  putStrLn "usage: crowdmatch YEAR MONTH DAY"
  putStrLn "e.g. crowdmatch 2019 09 01"
  exitWith (ExitFailure 2)

-- Show a prompt on the command line and return the response
prompt :: String -> IO String
prompt p = do
  putStr p
  hFlush stdout -- Make sure the prompt is printed before waiting for input
  getLine

-- In the future we should use ISO 8601 format dates. Newer versions of the
-- `time` library are able to parse them, so we can replace parseDay with
-- import Data.Time.Format.ISO8601 (iso8601ParseM)
-- However, the `time` library is a common dependency so we can't just bump
-- its version; we need to update all of our deps.. which we ought to do anyway,
-- but will take more time (heh) than I want to spend right now.
parseDay :: String -> String -> String -> Maybe Day
parseDay yyyy mm dd = do
  year <- readMaybe yyyy
  month <- readMaybe mm
  day <- readMaybe dd
  return (fromGregorian year month day)

parseDate :: String -> String -> String -> IO Day
parseDate yyyy mm dd = do
  date <- maybe usage return (parseDay yyyy mm dd)
  today <- fmap utctDay getCurrentTime
  if date <= today
    then return date
    else do
      putStrLn ("ERR: DATE must not be in the future. Provided date was: " ++ show date)
      exitWith (ExitFailure 3)

-- NB! The string passed to runPersistKeter must match the APPNAME used in
-- keter.sh to deploy the app. Must fix.
runCrowdmatch :: Day -> IO ()
runCrowdmatch day = runPersistKeter "SnowdriftReboot" (crowdmatch day)

-- This is pretty terrible FP form. That's intentional, to be more
-- approachable to non-Haskellers. This is also why parentheses are
-- preferred over $ for setting the order of operations.
main :: IO ()
main = do
  args <- getArgs
  (options, params) <- return (partition startsWithDash args)
  case params of
    yyyy : mm : dd : [] -> do
      day <- parseDate yyyy mm dd
      case options of
        "-y" : _ -> do
          putStrLn ("Running a crowdmatch on " ++ show day)
          runCrowdmatch day
        _ -> do
          response <- prompt ("A crowdmatch will be run on " ++ show day ++ ". Do you want to proceed (Y/n)? ")
          -- Change this if we ever support more than one argument.
          if response == "Y" || response == "y"
            then runCrowdmatch day
            else exitWith (ExitFailure 4)
    _ -> usage
  where
    startsWithDash letters = (take 1 letters) == "-"
