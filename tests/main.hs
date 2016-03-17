{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Import
import TestImport

import Test.Hspec (hspec)
import Yesod.Default.Config


import Application (makeFoundation)

-- All test modules
import UserTest

main :: IO ()
main = do
    conf <- Yesod.Default.Config.loadConfig $ (configSettings Testing)
                { csParseExtra = parseExtra
                }
    foundation <- makeFoundation conf
    spec foundation

spec :: App -> IO ()
spec foundation =
    hspec $ do
        yesodSpec foundation $ do
            userSpecs
