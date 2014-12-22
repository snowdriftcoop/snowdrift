{-# LANGUAGE LambdaCase #-}

module Main where

import qualified Data.ByteString.Char8 as C
import           System.Environment          (getArgs)
import           System.IO                   (stdin)
import           Data.Monoid                 ((<>))

main :: IO ()
main = getArgs >>= \case
    []     -> C.hGetContents stdin >>= C.putStrLn
    [file] -> C.hGetContents stdin >>= C.writeFile file
    args   -> error $ "invalid arguments: " <> unwords args