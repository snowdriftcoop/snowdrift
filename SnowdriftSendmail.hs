{-# LANGUAGE LambdaCase #-}

module Main where

import qualified Codec.MIME.QuotedPrintable as QuotedPrintable
import           Data.ByteString.Char8      (ByteString)
import qualified Data.ByteString.Char8      as C
import           System.Environment         (getArgs)
import           System.IO                  (stdin)
import           Data.Monoid                ((<>))

main :: IO ()
main = getArgs >>= \case
    []     -> C.hGetContents stdin >>= C.putStrLn . decode
    [file] -> C.hGetContents stdin >>= C.writeFile file . decode
    args   -> error $ "invalid arguments: " <> unwords args

decode :: ByteString -> ByteString
decode = C.pack . QuotedPrintable.decode . C.unpack
