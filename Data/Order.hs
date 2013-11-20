module Data.Order (parseOrderExpression, Orderable (..)) where

import Import

import Control.Applicative
import Data.Attoparsec.Text as A

-- import Data.Time

data Orderable = Orderable
    { hasTag :: Text -> Bool
    , getNamedTs :: Text -> Set UTCTime
    , searchLiteral :: Text -> Bool
    }

parseOrderExpression ::Text -> Either String (Orderable -> [Double])
parseOrderExpression "" = Right $ const [0]
parseOrderExpression expr = parseOnly seqP expr

c :: (b -> b -> b) -> (a -> b) -> (a -> b) -> a -> b
c op a b = \ x -> a x `op` b x

stripP :: Parser a -> Parser a
stripP p = let ws = A.takeWhile (inClass " \t") in ws *> p <* ws

seqP :: Parser (Orderable -> [Double])
seqP = (\ fs ord -> map ($ ord) fs) <$> seqP'
    where seqP' = (:) <$> expressionP <* stripP ";" <*> seqP' <|> return <$> expressionP

expressionP :: Parser (Orderable -> Double)
expressionP = stripP $ sumTermP

sumTermP :: Parser (Orderable -> Double)
sumTermP = foldl (flip ($)) <$> prodTermP <*> many (stripP sumOrDiffP <*> stripP prodTermP)
    where
        sumOrDiffP :: Parser ((Orderable -> Double) -> (Orderable -> Double) -> (Orderable -> Double))
        sumOrDiffP = ("+" >> return (flip (c (+)))) <|> (stripP "-" >> return (flip (c (-))))

prodTermP :: Parser (Orderable -> Double)
prodTermP = stripP $ foldl (c (*)) <$> divTermP <*> many (stripP "*" *> divTermP)

divTermP :: Parser (Orderable -> Double)
divTermP = stripP $ foldl (c (/)) <$> expTermP <*> many (stripP "/" *> expTermP)

expTermP :: Parser (Orderable -> Double)
expTermP = stripP $ foldl (c (**)) <$> termP <*> many (stripP "^" *> termP)

termP :: Parser (Orderable -> Double)
termP = stripP $
    tagP
    <|> const <$> double
    -- TODO <|> timeValueP
    <|> "(" *> expressionP <* ")"

{-
timeConstraintP :: Parser (Orderable -> [Double])
timeConstraintP =
    foldl1 (<|>) $ [before, after, between] <*> ["CREATED"]
    where
        before name = (\ end x -> not $ S.null $ fst $ S.split end $ getNamedTs x name) <$> (A.string name *> stripP "BEFORE" *> timeP)
        after name = (\ start x -> not $ S.null $ snd $ S.split start $ getNamedTs x name) <$> (A.string name *> stripP "AFTER" *> timeP)
        between name = (\ start end x -> not $ S.null $ snd $ S.split start $ fst $ S.split end $ getNamedTs x name) <$> (A.string name *> stripP "BETWEEN" *> timeP <* stripP "AND") <*> timeP 
-}

tagP :: Parser (Orderable -> Double)
tagP = (\ x y -> if hasTag y x then 1 else 0) <$> takeWhile1 (inClass "a-z-")

