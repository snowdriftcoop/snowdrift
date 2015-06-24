module Widgets.Search where

import Import
import qualified Data.Text as T

data SearchParameters = SearchParameters
    { claimed :: Bool
    , unclaimed :: Bool
    , tags :: Maybe Text
    , sort :: Maybe Text
    }


searchWidget :: Html -> MForm Handler (FormResult SearchParameters, Widget)
searchWidget extra = do
    (claimedRes, claimedView) <- mreq checkBoxField "Claimed" Nothing
    (unclaimedRes, unclaimedView) <- mreq checkBoxField "Unclaimed" Nothing
    (tagsRes, tagsView) <- mopt textField "Tags" Nothing
    (sortRes, sortView) <- mopt textField "Sort" Nothing

    let searchRes = SearchParameters 
                    <$> claimedRes
                    <*> unclaimedRes
                    <*> tagsRes
                    <*> sortRes

    let widget = do
          [whamlet|
            #{extra}
            <div>
              <p>^{fvInput claimedView} Claimed
                 ^{fvInput unclaimedView} Unclaimed
              <p>Tags: ^{fvInput tagsView}
            <div>
              <p>Sort: ^{fvInput sortView}
          |]

    return (searchRes, widget)

searchFilterString :: SearchParameters -> Text
searchFilterString (SearchParameters claimed unclaimed tags _ ) = 
    if (claimed)
        then
            if (unclaimed)
                then T.append T.empty $ parseTags tags
                else T.append "CLAIMED AND " $ parseTags tags 
        else
            if (unclaimed)
                then T.append "UNCLAIMED AND " $ parseTags tags
                else T.append T.empty $ parseTags tags
    where
        parseTags t = T.replace " " " AND " (fromMaybe T.empty t)

searchSortString :: SearchParameters -> Text
searchSortString (SearchParameters _ _ _ sortString) = fromMaybe T.empty sortString
