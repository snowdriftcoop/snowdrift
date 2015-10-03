module Data.Filter (defaultFilter, parseFilterExpression, Filterable (..)) where

import Import

import Control.Applicative
import Data.Attoparsec.Text as A
import Data.Time
import qualified Data.Set as S

-- TODO: allow for building custom SQL queries based on filters

data Filterable = Filterable
    { isClaimed :: Text -> Bool
    , hasTag :: Text -> Bool
    , getNamedTs :: Text -> Set UTCTime
    , searchLiteral :: Text -> Bool
    }

defaultFilter :: Filterable -> Bool
defaultFilter = const True

parseFilterExpression :: Text -> Either String (Filterable -> Bool)
parseFilterExpression "" = Right $ const True
parseFilterExpression expr = parseOnly expressionP expr

c :: (Bool -> Bool -> Bool) -> (a -> Bool) -> (a -> Bool) -> a -> Bool
c op a b x = a x `op` b x

stripP :: Parser a -> Parser a
stripP p = let ws = A.takeWhile (inClass " \t") in ws *> p <* ws

expressionP :: Parser (Filterable -> Bool)
expressionP = stripP orTermP

orTermP :: Parser (Filterable -> Bool)
orTermP = stripP $ foldl (c (||)) <$> andTermP <*> many (orP *> andTermP)

andTermP :: Parser (Filterable -> Bool)
andTermP = stripP $ foldl (c (&&)) <$> notTermP <*> many (andP *> notTermP)

notTermP :: Parser (Filterable -> Bool)
notTermP = stripP $ (not.) <$> (notP *> termP) <|> termP

termP :: Parser (Filterable -> Bool)
termP = stripP $
    claimedP
    <|> unclaimedP
    <|> tagP
    <|> timeConstraintP
    <|> "(" *> expressionP <* ")"

claimedP :: Parser (Filterable -> Bool)
claimedP = flip isClaimed <$> stripP "CLAIMED"

unclaimedP :: Parser (Filterable -> Bool)
unclaimedP = (\x y -> not $ isClaimed y x) <$> stripP "UNCLAIMED"

timeConstraintP :: Parser (Filterable -> Bool)
timeConstraintP =
    foldl1 (<|>) $ [before, after, between] <*> ["CREATED", "LAST UPDATED"]
    where
        before name = (\end x -> not $ S.null $ fst $ S.split end $ getNamedTs x name) <$> (A.string name *> stripP "BEFORE" *> timeP)
        after name = (\start x -> not $ S.null $ snd $ S.split start $ getNamedTs x name) <$> (A.string name *> stripP "AFTER" *> timeP)
        between name = (\start end x -> not $ S.null $ snd $ S.split start $ fst $ S.split end $ getNamedTs x name) <$> (A.string name *> stripP "BETWEEN" *> timeP <* stripP "AND") <*> timeP

timeP :: Parser UTCTime
timeP = fmap (`UTCTime` 0) $ stripP $ fromGregorian <$> (read <$> A.count 4 digit) <* "-" <*> (read <$> A.count 2 digit) <* "-" <*> (read <$> A.count 2 digit)

tagP :: Parser (Filterable -> Bool)
tagP = flip hasTag <$> takeWhile1 (inClass "a-z-")

andP :: Parser ()
andP = void $ stripP $ "^" <|> "AND"

orP :: Parser ()
orP = void $ stripP $ "v" <|> "OR"

notP :: Parser ()
notP = void $ "~" <|> "NOT"

