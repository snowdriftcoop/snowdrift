{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Crowdmatch
import RunPersist
import Data.Time (utctDay, getCurrentTime)
--- Import for a crowdmatch on a particular day, see comment below
-- import Data.Time (fromGregorian)

-- NB! The string passed to runPersistKeter must match the APPNAME used in
-- keter.sh to deploy the app. Must fix.
main :: IO ()
main = do
  --- Replace the line below with this one in order to manually specify the date
  --- on which the crowdmatch is to be run.  Otherwise (that's what the line
  --- below does), the current day (when you run the code) is used.
  -- d <- return $ fromGregorian 2019 09 01
  d <- utctDay <$> getCurrentTime
  runPersistKeter "SnowdriftReboot" $ crowdmatch d
