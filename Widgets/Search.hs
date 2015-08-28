{-# LANGUAGE OverloadedStrings #-}

module Widgets.Search where

import Import hiding (parseTime)
import qualified Data.Text as T
import Debug.Trace

data FilterClaimStatus = Claimed | Unclaimed | All deriving Eq

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

searchForm :: [Text] -> Form SearchParameters
searchForm tags extra = do
    let claimedList = [("All" :: Text, All),
                    ("Claimed", Claimed),
                    ("Unclaimed", Unclaimed)]

    (claimedRes, claimedView) <- mreq 
        (radioFieldList claimedList)
        "Claimed"
        (Just All)
    (timeRes, timeView) <- mreq timeWidget "Time" Nothing
    (tagsRes, tagsView) <- mreq (tagWidget tags) "Tags" Nothing
    (sortRes, sortView) <- mopt textField "Sort" Nothing

    let searchRes = SearchParameters 
                    <$> claimedRes
                    <*> timeRes
                    <*> tagsRes
                    <*> sortRes

    let widget = do
          [whamlet|
            #{extra}
            <div>
              <p>^{fvInput claimedView}
              <p>time:
              ^{fvInput timeView}
              <p>tags:
              ^{fvInput tagsView}
            <div>
              <p>Sort: ^{fvInput sortView}
          |]

    return (searchRes, widget)

timeWidget :: Field Handler TimeParameters
timeWidget = Field
    { fieldParse = \timeSelectValues _ ->
        trace "timeWidget fieldParse\n"
        (case timeSelectValues of
            [a, b, c, d] -> (return $ Right $ Just $ TimeParameters a b c d)
            _ -> return $ Left "Error with time selection: Missing/Too Much input.")
    , fieldView = \_ _ _ _ _ -> $(widgetFile "time_widget")
    , fieldEnctype = UrlEncoded
    }

tagWidget :: [Text] -> Field Handler [(Text, Text)]
tagWidget tags = Field
    { fieldParse = \tagSelectValues _ ->
          trace "tagWidget fieldParse\n"
            (if (length tagSelectValues > 0) then
                traceShow (zip tags tagSelectValues)
                (return $ Right $ Just $ zip tags tagSelectValues)
             else
                return $ Left "Error with tag selection: No input.")
    , fieldView = \_ _ _ _ _ -> $(widgetFile "tag_widget")
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
