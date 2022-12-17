{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Monad.IO.Class (liftIO)
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)
import Data.ByteString (ByteString)
import Data.ByteString.Base64 (decode)
import System.Environment (getArgs)
import System.Exit (ExitCode(ExitFailure, ExitSuccess), exitWith)
import System.IO (hPrint, stdout, stderr)
import Yesod.Auth.Util.PasswordStore (makePassword, verifyPassword)

parsePasswordOrExit :: String -> IO ByteString
parsePasswordOrExit input = do
  let maybePassword = decode (encodeUtf8 (pack input))
  case maybePassword of
    Left error -> do
      liftIO (hPrint stderr ("Error: invalid base64 encoding: " ++ error))
      exitWith (ExitFailure 2)
    Right password -> return password

main :: IO ()
main = do
  args <- getArgs
  case args of
    "check" : base64Password : digest : [] -> do
      password <- parsePasswordOrExit base64Password
      if verifyPassword password (encodeUtf8 (pack digest))
        then exitWith ExitSuccess
        else exitWith (ExitFailure 3)
    "create" : base64Password : [] -> do
      password <- parsePasswordOrExit base64Password
      digest <- liftIO (makePassword password 18)
      liftIO (hPrint stdout digest)
      exitWith ExitSuccess
    _ -> do
      liftIO (hPrint stderr "Invalid arguments. USAGE: 'create <base64Password>' or 'check <base64Password> <digest>'")
      exitWith (ExitFailure 1)
