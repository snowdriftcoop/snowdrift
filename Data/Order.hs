module Data.Order (parseOrderExpression, Orderable (..)) where

import Import

import Control.Applicative
import Data.Attoparsec.Text as A

import Data.Time

import qualified Data.Set as S

-- import Data.Time

data Orderable = Orderable
    { hasTag :: Text -> Bool
    , getNamedTs :: Text -> Set UTCTime
    , searchLiteral :: Text -> Bool
    }

epoch :: UTCTime
epoch = read "1970-01-01 00:00:00"

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
    <|> timeValueP
    <|> "(" *> expressionP <* ")"


toTimeValue :: UTCTime -> Double
toTimeValue = (/ 86400 {- seconds per day -}) . fromIntegral . (id :: Integer -> Integer) . round . diffUTCTime epoch

timeValueP :: Parser (Orderable -> Double)
timeValueP = timeConstraintP <|> fmap (const . toTimeValue) timeP

timeConstraintP :: Parser (Orderable -> Double)
timeConstraintP =
    foldl1 (<|>) $ [before, after, between, time] <*> ["CREATED"]
    where
        before name = do
            void $ A.string name
            void $ stripP "BEFORE"
            end <- timeP
            return $ \ x -> fromIntegral $ fromEnum $ not $ S.null $ fst $ S.split end $ getNamedTs x name

        after name = do
            void $ A.string name
            void $ stripP "AFTER"
            start <- timeP
            return $ \ x -> fromIntegral $ fromEnum $ not $ S.null $ snd $ S.split start $ getNamedTs x name

        between name = do
            void $ A.string name
            void $ stripP "BETWEEN"
            start <- timeP
            void $ stripP "AND"
            end <- timeP
            return $ \ x -> fromIntegral $ fromEnum $ not $ S.null $ snd $ S.split start $ fst $ S.split end $ getNamedTs x name

        time name = do
            void $ A.string name
            void $ stripP "TIME"
            return $ \ x -> maybe (-1) (toTimeValue . fst) $ S.maxView $ getNamedTs x name

timeP :: Parser UTCTime
timeP = fmap (`UTCTime` 0) $ stripP $ fromGregorian <$> (read <$> A.count 4 digit) <* "-" <*> (read <$> A.count 2 digit) <* "-" <*> (read <$> A.count 2 digit)


tagP :: Parser (Orderable -> Double)
tagP = (\ x y -> if hasTag y x then 1 else 0) <$> takeWhile1 (inClass "a-z-")

