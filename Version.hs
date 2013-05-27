module Version (mkVersion) where

import Import

import System.IO
import System.IO.Temp
import System.Cmd
import System.Exit

import Language.Haskell.TH

import Data.Char


getVersion :: IO (String, String)
getVersion = withSystemTempFile "version" $ \ filename handle -> do
    hClose handle
    base <- takeWhile (not . isSpace) <$> readFile ".git/refs/heads/master"
    ExitSuccess <- system $ "git diff " ++ base ++ " -- . >" ++ filename
    diff <- readFile filename
    return (base, diff)
 

mkVersion :: Q Exp
mkVersion = do
    (base, diff) <- runIO  getVersion
    return $ TupE $ map (LitE . StringL) $ [ base, diff ]

