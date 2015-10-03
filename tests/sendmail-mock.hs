{-# LANGUAGE LambdaCase #-}

module Main where

import Data.ByteString.Char8 (ByteString)
import Data.Monoid ((<>))
import System.Environment (getArgs)
import System.IO (stdin)
import qualified Codec.MIME.QuotedPrintable as QuotedPrintable
import qualified Data.ByteString.Char8 as C

main :: IO ()
main = getArgs >>= \case
    []     -> C.hGetContents stdin >>= C.putStrLn . decode
    [file] -> C.hGetContents stdin >>= C.writeFile file . decode
    args   -> error $ "invalid arguments: " <> unwords args

decode :: ByteString -> ByteString
decode = C.pack . QuotedPrintable.decode . C.unpack
