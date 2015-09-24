module Model.Currency where

import Control.Lens (over, both)
import Data.Int (Int64)
import Data.List (dropWhileEnd, intercalate)
import Data.Monoid ((<>))
import qualified Data.Text as T

import Prelude

import Database.Persist
import Database.Persist.Sql

import Text.Blaze

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

($*) :: Milray -> Double -> Milray
(Milray a) $* b = Milray $ floor $ fromIntegral a * b

(*$) :: Double -> Milray -> Milray
(*$) = flip ($*)

instance Num Milray where
    (Milray a) + (Milray b) = Milray $ a + b
    (Milray a) - (Milray b) = Milray $ a - b
    (*) = error "Cannot multiply money by money."

    signum (Milray a) = Milray $ signum a
    abs (Milray a) = Milray $ abs a
    fromInteger a = Milray $ fromInteger a

instance Show Milray where
    show = show . toCurrency

instance ToMarkup Milray where
    toMarkup = toMarkup . show

-- TODO: Define 'newtype Mill = Int64' and use it instead of 'Int64'.
millMilray :: Int64 -> Milray
millMilray = Milray . (10 *)

data Sign = NoSign | MinusSign

instance Show Sign where
    show NoSign    = ""
    show MinusSign = "-"

newtype IntPart  = IntPart String

instance Show IntPart where
    show (IntPart i) = i

newtype FracPart = FracPart String

instance Show FracPart where
    show (FracPart f) = f

data Currency = Dollar Sign IntPart FracPart
              | Cent Sign IntPart FracPart

instance Show Currency where
    show (Dollar s i f) =
        "$" <> show s <> show i <> "." <> show f
    show (Cent   s i f) =
        show s <> show i <> (if null sf then "" else "." <> sf) <> "¢"
      where
        sf = show f

sign :: Int64 -> Sign
sign i = if i < 0 then MinusSign else NoSign

dropRightZeros :: String -> String
dropRightZeros = dropWhileEnd (== '0')

splitByThree :: [a] -> [[a]]
splitByThree [] = []
splitByThree s  = case splitAt 3 s of
    (a, b) -> a : splitByThree b

pprintThousands :: String -> String
pprintThousands =
    reverse . intercalate "," . splitByThree . reverse

-- Always show at least two fractional digits for dollars, and drop
-- zeros for cents, like so:
--
-- 1 milray     = 0.01¢
-- 10 milray    = 0.1¢
-- 100 milray   = 1¢
-- 1000 milray  = 10¢
-- 10000 milray = $1.00
toCurrency :: Milray -> Currency
toCurrency (Milray m)
    | m == 0 =
      Dollar NoSign (IntPart "0") (FracPart "00")
    | am > 0 && am < 10 =         -- [1..9], 1 digit, milray
      Cent (sign m) (IntPart "0") (FracPart $ "0" <> sam)
    | am >= 10 && am < 100 =      -- [10..99], 2 digits, mill
      Cent (sign m) (IntPart "0") (FracPart $ dropRightZeros sam)
    | am >= 100 && am < 1000 =    -- [100..999], 3 digits, cent
      let (i,f) = splitAt 1 sam
      in Cent (sign m) (IntPart i) (FracPart $ dropRightZeros f)
    | am >= 1000 && am < 10000 =  -- [1000..9999], 4 digits, cent
      let (i,f) = splitAt 2 sam
      in Cent (sign m) (IntPart i) (FracPart $ dropRightZeros f)
    | otherwise =                 -- 5 or more digits, dollars
      let (f,i) = over both reverse $ splitAt 4 $ reverse sam
          -- Always print at least two fractional digits.
          (cents, milrays) = splitAt 2 f
      in Dollar
             (sign m)
             (IntPart $ pprintThousands i)
             (FracPart $ cents <> dropRightZeros milrays)
  where
    am  = abs m
    sam = show am
