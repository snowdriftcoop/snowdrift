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

instance Currency Cent where
    (Cent a) $* b = Cent $ floor $ fromIntegral a * b
    b *$ (Cent a) = Cent $ floor $ fromIntegral a * b

dropLeftZeros :: String -> String
dropLeftZeros = dropWhile (== '0')

instance Show Cent where
    show (Cent c) =
        if null ipart
        then if null fpart'
             then "$0.00"
             else sign ++ fpart' ++ "¢"
        else "$" ++ sign ++ ipart ++ "." ++ fpart
      where
        splitPieces [] = []
        splitPieces s = case splitAt 3 s of (a, b) -> a : splitPieces b
        sign = if c < 0 then "-" else ""
        (f,i) = splitAt 2 $ reverse $ show $ abs c
        fpart = reverse $ take 2 $ f ++ repeat '0'
        fpart' = dropLeftZeros fpart
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

-- TODO: Define 'newtype Mill = Int64' and use it instead of 'Int64'.
millMilray :: Int64 -> Milray
millMilray = Milray . (10 *)
