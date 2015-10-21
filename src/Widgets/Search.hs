{-# LANGUAGE OverloadedStrings #-}

module Widgets.Search
    ( searchForm
    , searchFilterString
    , searchSortString
    ) where

import Import

import qualified Data.Text as T

data FilterClaimStatus = Claimed
                       | Unclaimed
                       | All
                       deriving (Show, Eq)

data TagStatus = TagIncluded
               | TagExcluded
               | TagNeither
               deriving Eq

data SearchParameters = SearchParameters
    { _claimed :: FilterClaimStatus
    , _tags :: [(Text, TagStatus)]
    , _sort :: Maybe Text
    }

convertTagStatus :: Text -> TagStatus
convertTagStatus tag =
        case tag of
          "include" -> TagIncluded
          "exclude" -> TagExcluded
          _ -> TagNeither

searchForm :: [Text] -> Form SearchParameters
searchForm tags extra = do
    let claimedList =
            [ ("All" :: Text, All)
            , ("Claimed", Claimed)
            , ("Unclaimed", Unclaimed)]

    (claimedRes, claimedView) <- mreq
        (selectField $ optionsPairs claimedList)
        (FieldSettings "Claimed"
                       Nothing
                       Nothing
                       Nothing
                       [("class", "form-control")])
        (Just All)
    (tagsRes, tagsView) <- mreq (tagField tags) "Tags" Nothing
    (sortRes, sortView) <- mopt textField "Sort" Nothing

    let searchRes = SearchParameters
                    <$> claimedRes
                    <*> tagsRes
                    <*> sortRes

    let widget = toWidget $(widgetFile "form/ticket-filter")

    return (searchRes, widget)

tagField :: [Text] -> Field Handler [(Text, TagStatus)]
tagField tags = Field
    { fieldParse = \tagSelectValues _ -> return $
                                            Right $
                                            Just $
                                            zip tags $
                                            map convertTagStatus tagSelectValues
    , fieldView = \_ fieldName _ fieldVal _ ->
            $(widgetFile "field/tag-filter")
    , fieldEnctype = UrlEncoded
    }

searchFilterString :: SearchParameters -> Text
searchFilterString (SearchParameters Claimed tags _ ) =
        T.intercalate " AND " $ filter (not . T.null)
            [T.pack "CLAIMED", parseTags tags]
searchFilterString (SearchParameters Unclaimed tags _ ) =
        T.intercalate " AND " $ filter (not . T.null)
            [T.pack "UNCLAIMED", parseTags tags]
searchFilterString (SearchParameters All tags _) =
        T.intercalate " AND " $ filter (not . T.null) [parseTags tags]

parseTags :: [(Text, TagStatus)] -> Text
parseTags t = T.intercalate " AND " $ filter (not . T.null) (map parseTag t)
    where parseTag (tag, response)
              | (tag == T.empty) = T.empty
              | (response == TagIncluded) = "(" <> tag <> ")"
              | (response == TagExcluded) = "(NOT " <> tag <> ")"
              | otherwise = T.empty

searchSortString :: SearchParameters -> Text
searchSortString (SearchParameters _ _ sortString) = fromMaybe
                                                         T.empty
                                                         sortString
