{-# LANGUAGE CPP #-}

#if MIN_VERSION_base(4,8,0)
{-# LANGUAGE FlexibleContexts #-}
#endif

module Model.Currency where

import Data.Int (Int64)
import Data.List (intercalate)
import qualified Data.Text as T

import Prelude

import Database.Persist
import Database.Persist.Sql

import Text.Blaze


class Currency a where
   ($*) :: a -> Double -> a
   (*$) :: Double -> a -> a

data Cent = Cent Int64 deriving (Eq, Ord)

instance Show Cent where
    show (Cent c) =
        "$" ++ sign ++ (if null ipart then "0" else ipart) ++ "." ++ fpart
      where
        splitPieces [] = []
        splitPieces s = case splitAt 3 s of (a, b) -> a : splitPieces b
        sign = if c < 0 then "-" else ""
        (f,i) = splitAt 2 $ reverse $ show $ abs c
        fpart = reverse $ take 2 $ f ++ repeat '0'
        ipart = intercalate "," $ reverse $ map reverse $ splitPieces i

instance ToMarkup Cent where
    toMarkup = toMarkup . show

milrayCents :: Milray -> Cent
milrayCents (Milray a) = Cent (round $ fromIntegral a / (100 :: Double))

{-
 - Milray - 1/100th of 1 cent
 -  from "A Connecticut Yankee In King Arthur's Court"
 -}

data Milray = Milray Int64 deriving (Eq, Ord)

instance PersistField Milray where
    toPersistValue (Milray i) = PersistInt64 i
    fromPersistValue (PersistInt64 i) = Right $ Milray i
    fromPersistValue x =
        Left $ T.pack $ "Expected Integer, received: " ++ show x

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
    show (Milray m) =
        "$" ++ sign ++ (if null ipart then "0" else ipart) ++ "." ++ fpart
      where
        splitPieces [] = []
        splitPieces s = case splitAt 3 s of (a, b) -> a : splitPieces b
        sign = if m < 0 then "-" else ""
        (f,i) = splitAt 4 $ reverse $ show $ abs m
        fpart = reverse $ take 4 $ f ++ repeat '0'
        ipart = intercalate "," $ reverse $ map reverse $ splitPieces i

instance ToMarkup Milray where
    toMarkup = toMarkup . show

instance Read Milray where
    readsPrec p ('$':'-':s) = map (\ (Milray i, rest) -> (Milray $ -i, rest)) $ readsPrec p ('$':s)
    readsPrec _ ('$':s) = let (ipart, more) = 
#if MIN_VERSION_base(4,8,0)
                                span (`elem` (",0123456789" :: String)) s
#else
                                span (`elem` (",0123456789")) s
#endif
                              (fpart, rest) = case more of 
                                                  '.' : more' -> 
#if MIN_VERSION_base(4,8,0)
                                                        span (`elem` ("0123456789" :: String)) more'
#else
                                                        span (`elem` "0123456789") more'
#endif
                                                  _ -> ("", more)
                                              
                              result = Milray $ read $ filter (/= ',') ipart ++ take 4 (fpart ++ repeat '0')
                           in [ (result, rest) ]

    readsPrec _ _ = []


