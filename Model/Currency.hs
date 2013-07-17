module Model.Currency where

import Data.Int (Int64)
import Data.List (intercalate)
import qualified Data.Text as T

import Prelude

import Database.Persist
import Database.Persist.Sql


class Currency a where
   ($*) :: a -> Double -> a
   (*$) :: Double -> a -> a

{-
 - Milray - 1/100th of 1 cent
 -  from "A Connecticut Yankee In King Arthur's Court"
 -}

data Milray = Milray Int64 deriving (Eq, Ord)

instance PersistField Milray where
    toPersistValue (Milray i) = PersistInt64 i
    fromPersistValue (PersistInt64 i) = Right $ Milray i
    fromPersistValue x = Left $ T.pack $ "Expected Integer, received: " ++ show x

instance PersistFieldSql Milray where
    sqlType _ = SqlInt64

instance Currency Milray where
    (Milray a) $* b = Milray $ floor $ fromIntegral a * b
    b *$ (Milray a) = Milray $ floor $ fromIntegral a * b

instance Num Milray where
    (Milray a) + (Milray b) = Milray $ a + b
    (Milray a) - (Milray b) = Milray $ a - b
    (*) = error "Cannot multiply money by money."

    signum (Milray a) = Milray $ signum a
    abs (Milray a) = Milray $ abs a
    fromInteger a = Milray $ fromInteger a

instance Show Milray where
    show (Milray m) = let splitPieces [] = []
                          splitPieces s = case splitAt 3 s of (a, b) -> a : splitPieces b
                          sign = if m < 0 then "-" else ""
                          (f,i) = splitAt 4 $ reverse $ show $ abs m
                          fpart = reverse $ take 4 $ f ++ repeat '0'
                          ipart = intercalate "," $ reverse $ map reverse $ splitPieces i
                      in "$" ++ sign ++ (if null ipart then "0" else ipart) ++ "." ++ fpart

instance Read Milray where
    readsPrec p ('$':'-':s) = map (\ (Milray i, rest) -> (Milray $ -i, rest)) $ readsPrec p ('$':s)
    readsPrec _ ('$':s) = let (ipart, more) = span (`elem` ",0123456789") s
                              (fpart, rest) = case more of 
                                                  '.' : more' -> span (`elem` "0123456789") more'
                                                  _ -> ("", more)
                                              
                              result = Milray $ read $ filter (/= ',') ipart ++ take 4 (fpart ++ repeat '0')
                           in [ (result, rest) ]

    readsPrec _ _ = []


