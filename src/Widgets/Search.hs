{-# LANGUAGE OverloadedStrings #-}

module Widgets.Search where

import Import hiding (parseTime, timeField)
import Data.Char
import qualified Data.Text as T

data FilterClaimStatus = Claimed
                       | Unclaimed
                       | All
                       deriving (Show, Eq)

data DateTypes = DateTypeBlank
               | DateCreated
               | DateUpdated
               deriving Eq

data DateParams = DateParamBlank
                | DateBefore
                | DateAfter
                | DateBetween
                deriving Eq

data DateStatus = DateType DateTypes
                | DateParam DateParams
                | DateOther Text
                deriving Eq

data TagStatus = Included
               | Excluded
               | Neither
               deriving Eq

data DateParameters = DateParameters
    { basedON :: DateTypes
    , criteria :: DateParams
    , startDate :: Text
    , endDate :: Text
    }

data SearchParameters = SearchParameters
    { claimed :: FilterClaimStatus
    , date :: DateParameters
    , tags :: [(Text, Text)]
    , sort :: Maybe Text
    }

fromDateType :: DateTypes -> Text
fromDateType x = case x of
               DateTypeBlank -> ""
               DateCreated -> "Created"
               DateUpdated -> "Updated"

toDateType :: Text -> DateTypes
toDateType x = case x of
                "Created" -> DateCreated
                "Updated" -> DateUpdated
                _ -> DateTypeBlank

fromDateParam :: DateParams -> Text
fromDateParam x = case x of
                DateParamBlank -> ""
                DateBefore -> "before"
                DateAfter -> "after"
                DateBetween -> "between"

toDateParam :: Text -> DateParams
toDateParam x = case x of
                  "before" -> DateBefore
                  "after" -> DateAfter
                  "between" -> DateBetween
                  _ -> DateParamBlank

textFromDateStatus :: DateStatus -> Text
textFromDateStatus x = case x of
                        DateType a -> fromDateType a
                        DateParam b -> fromDateParam b
                        DateOther c -> c

dateStatus :: Text -> DateParameters -> DateStatus
dateStatus tid responses = 
        case tid of
            "datetype"  -> DateType $ basedON responses
            "dateparam" -> DateParam $ criteria responses
            "datestart" -> DateOther $ startDate responses
            "dateend"   -> DateOther $ endDate responses
            _ -> DateOther $ T.empty

tagStatus :: Text -> [(Text, Text)] -> TagStatus
tagStatus tag responses =
        case (lookup tag responses) of
          Just "include" -> Included
          Just "exclude" -> Excluded
          _ -> Neither

searchForm :: [Text] -> Form SearchParameters
searchForm tags extra = do
    let claimedList = [("All" :: Text, All),
                    ("Claimed", Claimed),
                    ("Unclaimed", Unclaimed)]

    (claimedRes, claimedView) <- mreq 
        (selectField $ optionsPairs claimedList)
        (FieldSettings "Claimed" 
                       Nothing
                       Nothing
                       Nothing
                       [("class", "form-control")])
        (Just All)
    (dateRes, dateView) <- mreq validatedDateField "Time" Nothing
    (tagsRes, tagsView) <- mreq (tagField tags) "Tags" Nothing
    (sortRes, sortView) <- mopt textField "Sort" Nothing

    let searchRes = SearchParameters 
                    <$> claimedRes
                    <*> dateRes
                    <*> tagsRes
                    <*> sortRes

    let widget = toWidget $(widgetFile "filter-widget")

    return (searchRes, widget)
  where
    errorMessage :: Text
    errorMessage = T.append
                     "Date Created/Updated & before/after must be checked;"
                     " date value must be in a recognized format."
    validatedDateField = checkBool validateDateField errorMessage dateField

validateDateField :: DateParameters -> Bool
validateDateField (DateParameters t p s _) 
    | (t == DateTypeBlank) && (p == DateParamBlank) = True
    | (t /= DateTypeBlank) &&
      (p /= DateParamBlank) &&
      ((valiDate $ T.splitOn "/" s) ||
       (valiDate $ T.splitOn "-" s)) = True
    | otherwise = False

-- Check to make sure the date is in an appropriate format.  Accepted formats:
--     mm/dd/yyyy
--     dd/mm/yyyy
--     yyyy/mm/dd
--     yyyy/dd/mm
valiDate :: [Text] -> Bool
valiDate [a, b, c]
  | T.length a == 2 = if ((((isMonth a) && (isDay b))
                         || ((isDay a) && (isMonth b)))
                         && (isYear c))
                       then True
                       else False
  | T.length a == 4 = if ((((isMonth b) && (isDay c))
                         || ((isDay b) && (isMonth c)))
                         && (isYear a))
                       then True
                       else False
  | otherwise = False
valiDate _ = False

toInt :: Text -> Int
toInt x = textToInt (T.takeWhile isDigit x) 0 0

textToInt :: Text -> Int -> Int -> Int
textToInt t accum place = case (T.length t) of
    0 -> accum
    _ -> (accum +
         ((digitToInt $ T.last t) * 10^place) +
          (textToInt (T.init t) accum (place+1)))

isMonth :: Text -> Bool
isMonth x = (toInt x) > 0 && (toInt x) < 12

isDay :: Text -> Bool
isDay x = (toInt x) > 0 && (toInt x) < 31

isYear :: Text -> Bool
isYear x = (toInt x) > 2000

convertDate :: Text -> Text
convertDate initdate
    | ((length (T.splitOn "/" initdate)) > 1) =
            convertDate2 (T.splitOn "/" initdate)
    | ((length (T.splitOn "-" initdate)) > 1) =
            convertDate2 (T.splitOn "-" initdate)
    | otherwise = "1901-01-01"

-- This will convert any input to the default yyyy-mm-dd to the best of its
-- ability.  If both month and day are less than 12, then it will assume the
-- first variable (a/b) is the month and the second (b/c) is the day.
-- Note #1:  Placeholder will say "YYYY-MM-DD", so these other scenarios are 
--        not very likely to happen anyway.
-- Note #2: Validation has occurred before this function is reached, so we can
--        be sure that one of the first two conditions should be matched. I
--        added the other two just in case something goes wrong.
convertDate2 :: [Text] -> Text
convertDate2 [a, b, c]
  | T.length a == 2 = if ((isMonth a) && (isMonth b))
                         then T.intercalate "-" [c, a, b]
                         else if ((isMonth a) && (isDay b))
                           then T.intercalate "-" [c, a, b]
                           else T.intercalate "-" [c, b, a]
  | T.length a == 4 = if ((isMonth b) && (isMonth c))
                         then T.intercalate "-" [a, b, c]
                         else if ((isMonth a) && (isDay b))
                            then T.intercalate "-" [a, b, c]
                            else T.intercalate "-" [a, c, b]
  | otherwise = "1901-01-01"
convertDate2 _ = "1901-01-01"


dateField :: Field Handler DateParameters
dateField = Field
    { fieldParse = \dateSelectValues _ ->
        case dateSelectValues of
            [a, b, c, d] -> return $
                                Right $
                                Just (DateParameters (toDateType a) 
                                                     (toDateParam b)
                                                     (c :: Text)
                                                     (d :: Text))
            _ -> return $
              Left "Error with time selection: Missing/Too Much input."
    , fieldView = \_ fieldName _ fieldVal _ ->
                    $(widgetFile "filter-datefield")
    , fieldEnctype = UrlEncoded
    }

tagField :: [Text] -> Field Handler [(Text, Text)]
tagField tags = Field
    { fieldParse = \tagSelectValues _ -> return $
                                            Right $
                                            Just $
                                            zip tags tagSelectValues
    , fieldView = \_ fieldName _ fieldVal _ ->
            $(widgetFile "filter-tagfield")
    , fieldEnctype = UrlEncoded
    }

searchFilterString :: SearchParameters -> Text
searchFilterString (SearchParameters Claimed searchdate tags _ ) = 
        T.intercalate " AND " $ filter (not . T.null)
            [(T.pack "CLAIMED"),
             (parseFilterDate searchdate),
             (parseTags tags)
            ]
searchFilterString (SearchParameters Unclaimed searchdate tags _ ) =
        T.intercalate " AND " $ filter (not . T.null)
            [(T.pack "UNCLAIMED"),
             (parseFilterDate searchdate),
             (parseTags tags)
            ]
searchFilterString (SearchParameters All searchdate tags _) =
        T.intercalate " AND " $ filter (not . T.null)
            [(parseFilterDate searchdate),
             (parseTags tags)
            ]

parseFilterDate :: DateParameters -> Text
parseFilterDate (DateParameters base crit start end)
        | (base == DateCreated) || (base == DateUpdated) = T.toUpper $
                if (crit == DateBetween) then
                    T.intercalate " " 
                        [(fromDateType base), 
                         (fromDateParam crit), 
                         (convertDate start),
                         "AND", 
                         (convertDate end)]
                else
                    T.intercalate " "
                        [(fromDateType base),
                         (fromDateParam crit),
                         (convertDate start)]
        | otherwise = T.empty

parseTags :: [(Text, Text)] -> Text
parseTags t = T.intercalate " AND " $ filter (not . T.null) (map parseTag t)
    where parseTag (tag, response)
              | (tag == T.empty) = T.empty
              | (response == "include") = "(" <> tag <> ")"
              | (response == "exclude") = "(NOT " <> tag <> ")"
              | otherwise = T.empty

searchSortString :: SearchParameters -> Text
searchSortString (SearchParameters _ _ _ sortString) = fromMaybe
                                                            T.empty
                                                            sortString
