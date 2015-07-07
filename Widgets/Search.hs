module Widgets.Search where

import Import
import qualified Data.Text as T

data FilterClaimStatus = Claimed | Unclaimed | All deriving Eq

data SearchParameters = SearchParameters
    { claimed :: FilterClaimStatus
    , tags :: Maybe Text
    , sort :: Maybe Text
    }

searchWidget :: Form SearchParameters
searchWidget extra = do
    let claimedList = [("All" :: Text, All),
                    ("Claimed", Claimed),
                    ("Unclaimed", Unclaimed)]
    (claimedRes, claimedView) <- mreq 
        (radioFieldList claimedList)
        "Claimed"
        (Just All)
    (tagsRes, tagsView) <- mopt textField "Tags" Nothing
    (sortRes, sortView) <- mopt textField "Sort" Nothing

    let searchRes = SearchParameters 
                    <$> claimedRes
                    <*> tagsRes
                    <*> sortRes

    let widget = do
          [whamlet|
            #{extra}
            <div>
              <p>^{fvInput claimedView}
              <p>Tags: ^{fvInput tagsView}
              <p>* Tags can be separated using AND, OR, NOT and ()
            <div>
              <p>Sort: ^{fvInput sortView}
          |]

    return (searchRes, widget)

searchFilterString :: SearchParameters -> Text
searchFilterString (SearchParameters Claimed tags _ ) = 
        T.append "CLAIMED AND " $ parseTags tags
searchFilterString (SearchParameters Unclaimed tags _ ) =
        T.append "UNCLAIMED AND " $ parseTags tags
searchFilterString (SearchParameters All tags _) =
        T.append T.empty $ parseTags tags

parseTags :: Maybe Text -> Text
parseTags t = fromMaybe T.empty t

searchSortString :: SearchParameters -> Text
searchSortString (SearchParameters _ _ sortString) = fromMaybe T.empty sortString
