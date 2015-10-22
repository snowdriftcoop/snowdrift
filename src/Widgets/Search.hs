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

data TagAction = TagInclude
               | TagExclude
               | TagIgnore
               deriving Eq

data SearchParameters = SearchParameters
    { _claimed :: FilterClaimStatus
    , _tags :: [(Text, TagAction)]
    , _sort :: Maybe Text
    }

convertTagAction :: Text -> TagAction
convertTagAction tag =
        case tag of
          "include" -> TagInclude
          "exclude" -> TagExclude
          _ -> TagIgnore

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

tagField :: [Text] -> Field Handler [(Text, TagAction)]
tagField tags = Field
    { fieldParse = \tagSelectValues _ -> return $
                                            Right $
                                            Just $
                                            zip tags $
                                            map convertTagAction tagSelectValues
    , fieldView = \idAttr nameAttr otherAttrs eResult isReq ->
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

parseTags :: [(Text, TagAction)] -> Text
parseTags t = T.intercalate " AND " $ filter (not . T.null) (map parseTag t)
    where parseTag (tag, response)
              | (tag == T.empty) = T.empty
              | (response == TagInclude) = "(" <> tag <> ")"
              | (response == TagExclude) = "(NOT " <> tag <> ")"
              | otherwise = T.empty

searchSortString :: SearchParameters -> Text
searchSortString (SearchParameters _ _ sortString) = fromMaybe
                                                         T.empty
                                                         sortString
