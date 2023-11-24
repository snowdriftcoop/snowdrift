{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Crowdmatch (crowdmatch)
import RunPersist (runPersistKeter)

import Control.Applicative ((<|>))
import Data.List (partition)
import Data.Time
import System.Environment (getArgs)
import System.Exit (ExitCode(ExitFailure), exitWith)
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

usage :: IO a -- exits
usage = do
  putStrLn "usage: crowdmatch <crowdmatch-date> [<check-time>]"
  putStrLn ""
  putStrLn "  Example:crowdmatch \"$(date -Idate --date 'last monday')\" \"$(date -Isec --date yesterday)\""
  putStrLn ""
  putStrLn "  <crowdmatch-date>: The date a crowdmatch is calculated on."
  putStrLn "  <check-time>:      Members who dropped before this date are not included."
  putStrLn "                     Default value: now."
  putStrLn ""
  putStrLn "  Crowdmatch date is in the format 2019-09-01"
  putStrLn ""
  putStrLn "  Check time is either in the same format as the crowdmatch date, or in the extended format 2019-09-01T00:00:00+0000"
  exitWith (ExitFailure 2)

-- Show a prompt on the command line and return the response
prompt :: String -> IO String
prompt p = do
  putStr p
  hFlush stdout -- Make sure the prompt is printed before waiting for input
  getLine

-- | Simple day parser.
parseDay :: String -> Maybe Day
parseDay = parseTimeM False defaultTimeLocale "%Y-%m-%d"

-- | Simple time parser.
-- If the time isn't specified, it defaults to 00:00:00, i.e. midnight at the
-- start of the day.
parseTime' :: String -> Maybe UTCTime
parseTime' s = (UTCTime <$> parseDay s <*> pure 0)
    <|> parseTimeM False defaultTimeLocale "%Y-%m-%dT%H:%M:%S%z" s

-- NB! The string passed to runPersistKeter must match the APPNAME used in
-- keter.sh to deploy the app. Must fix.
runCrowdmatch :: Day -> UTCTime -> IO ()
runCrowdmatch crowdDay checkTime = runPersistKeter "SnowdriftReboot" (crowdmatch crowdDay checkTime)

main :: IO ()
main = do
  args <- getArgs
  (options, params) <- return (partition startsWithDash args)
  case params of
    [crowd] -> do
        checkDate <- Just <$> getCurrentTime
        go options (parseDay crowd) checkDate
    [crowdStr, checkStr] ->
        go options (parseDay crowdStr) (parseTime' checkStr)
    _ -> usage
  where
    startsWithDash letters = (take 1 letters) == "-"

go :: [String] -> Maybe Day -> Maybe UTCTime -> IO ()
go _ Nothing _ = usage
go _ _ Nothing = usage
go options (Just crowd) (Just check) =
    if UTCTime crowd 0 > check then do
        putStrLn ("ERR: Crowdmatch date must be ≤ check date. Crowdmatch date was: " ++ show crowd ++ ", check date was: " ++ show check)
        exitWith (ExitFailure 3)
    else case options of
        "-y" : _ -> do
            putStrLn ("Running a crowdmatch on " ++ show crowd ++ " with check date " ++ show check)
            runCrowdmatch crowd check
        _ -> do
            response <- prompt ("A crowdmatch will be run on " ++ show crowd ++ "with check date " ++ show check ++ ". Proceed (Y/n)? ")
            -- Change this if we ever support more than one argument.
            if response == "Y" || response == "y"
                then runCrowdmatch crowd check
                else exitWith (ExitFailure 4)
