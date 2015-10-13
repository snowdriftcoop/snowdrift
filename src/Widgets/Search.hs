{-# LANGUAGE OverloadedStrings #-}

module Widgets.Search where

import Import hiding (parseTime, timeField)
import qualified Data.Text as T
import Debug.Trace

data FilterClaimStatus = Claimed | Unclaimed | All deriving (Show, Eq)

data TagStatus = Included | Excluded | Neither deriving (Eq)

data TimeParameters = TimeParameters
    { basedON :: Text
    , criteria :: Text
    , startTime :: Text
    , endTime :: Text
    }

data SearchParameters = SearchParameters
    { claimed :: FilterClaimStatus
    , time :: TimeParameters
    , tags :: [(Text, Text)]
    , sort :: Maybe Text
    }

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
        (radioFieldList claimedList)
        "Claimed"
        (Just All)
--    (timeRes, timeView) <- mreq timeField "Time" Nothing
    (tagsRes, tagsView) <- mreq (tagField tags) "Tags" Nothing
    (sortRes, sortView) <- mopt textField "Sort" Nothing

    let searchRes = SearchParameters 
                    <$> claimedRes
                    <*> pure (TimeParameters "" "" "" "")
--                    <*> timeRes
                    <*> tagsRes
                    <*> sortRes

    let widget = do
          [whamlet|
            #{extra}
            <div>
              <p>^{fvInput claimedView}
              <p>tags:
              ^{fvInput tagsView}
            <div>
              <p>Sort: ^{fvInput sortView}
          |]

    return (searchRes, widget)

timeField :: Field Handler TimeParameters
timeField = Field
    { fieldParse = \timeSelectValues _ ->
        trace ("timeWidget fieldParse\nlength of timeSelectValues: " ++ (show (length timeSelectValues)))
        (case timeSelectValues of
            [a, b, c, d] -> (return $ Right $ Just $ TimeParameters a b c d)
            _ -> return $ Left "Error with time selection: Missing/Too Much input.")
    , fieldView = \fieldId fieldName fieldAttrs fieldVal fieldReq -> $(widgetFile "time_widget")
    , fieldEnctype = UrlEncoded
    }

tagField :: [Text] -> Field Handler [(Text, Text)]
tagField tags = Field
    { fieldParse = \tagSelectValues _ ->
          trace ("tagWidget fieldParse\nlength of tagSelectValues: " ++ (show (length tagSelectValues)))
            (if (length tagSelectValues > 0) then
                traceShow (zip tags tagSelectValues)
                (return $ Right $ Just $ zip tags tagSelectValues)
             else
                return $ Left "Error with tag selection: No input.")
    , fieldView = \fieldId fieldName fieldAttrs fieldVal fieldReq -> $(widgetFile "tag_widget")
    , fieldEnctype = UrlEncoded
    }

searchFilterString :: SearchParameters -> Text
searchFilterString (SearchParameters Claimed time tags _ ) = 
        T.intercalate " AND " $ filter (not . T.null)
            [(T.pack "CLAIMED"),
             (parseTime time),
             (parseTags tags)
            ]
        -- T.append "CLAIMED AND " $ parseTags tags
searchFilterString (SearchParameters Unclaimed time tags _ ) =
        T.intercalate " AND " $ filter (not . T.null)
            [(T.pack "UNCLAIMED"),
             (parseTime time),
             (parseTags tags)
            ]
        --T.append "UNCLAIMED AND " $ parseTags tags
searchFilterString (SearchParameters All time tags _) =
        T.intercalate " AND " $ filter (not . T.null)
            [(parseTime time),
             (parseTags tags)
            ]
        --T.append T.empty $ parseTags tags

parseTime :: TimeParameters -> Text
parseTime (TimeParameters base crit start end)
        | (base == "Created") || (base == "Last Updated") = T.toUpper $
                if (crit == "between") then
                    T.intercalate " " [base, crit, start, "AND", end]
                else
                    T.intercalate " " [base, crit, start]
        | otherwise = T.empty

parseTags :: [(Text, Text)] -> Text
parseTags t = T.intercalate " AND " $ filter (not . T.null) (map parseTag t)
    where parseTag (tag, response)
              | (tag == T.empty) = T.empty
              | (response == "include") = "(" <> tag <> ")"
              | (response == "exclude") = "(NOT " <> tag <> ")"
              | otherwise = T.empty

searchSortString :: SearchParameters -> Text
searchSortString (SearchParameters _ _ _ sortString) = fromMaybe T.empty sortString
