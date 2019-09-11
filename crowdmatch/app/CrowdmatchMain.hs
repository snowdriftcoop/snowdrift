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
  putStrLn "usage: crowdmatch [YEAR MONTH]"
  putStrLn "e.g. crowdmatch 2019 09"
  exitWith $ ExitFailure 2

parseDate :: String -> String -> Maybe Day
parseDate y m = do
  year <- readMaybe y
  month <- readMaybe m
  return $ fromGregorian year month 1

-- NB! The string passed to runPersistKeter must match the APPNAME used in
-- keter.sh to deploy the app. Must fix.
main :: IO ()
main = do
  args <- getArgs
  d <- case args of
    [] -> utctDay <$> getCurrentTime
    y : m : [] -> maybe usage return $ parseDate y m
    _ -> usage
  runPersistKeter "SnowdriftReboot" $ crowdmatch d
