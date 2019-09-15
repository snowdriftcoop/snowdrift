{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Crowdmatch
import RunPersist

import Data.Time (Day, fromGregorian, getCurrentTime, utctDay)
import System.Environment (getArgs)
import System.Exit (ExitCode(ExitFailure), exitWith)
import Text.Read (readMaybe)

usage :: IO a -- exits
usage = do
  putStrLn "usage: crowdmatch [YEAR MONTH DAY]"
  putStrLn "e.g. crowdmatch 2019 09 01"
  exitWith $ ExitFailure 2

parseDate :: String -> String -> String -> Maybe Day
parseDate y m d = do
  year <- readMaybe y
  month <- readMaybe m
  day <- readMaybe d
  return $ fromGregorian year month day

-- NB! The string passed to runPersistKeter must match the APPNAME used in
-- keter.sh to deploy the app. Must fix.
main :: IO ()
main = do
  args <- getArgs
  day <- case args of
    [] -> utctDay <$> getCurrentTime
    y : m : d : [] -> maybe usage return $ parseDate y m d
    _ -> usage
  runPersistKeter "SnowdriftReboot" $ crowdmatch day
