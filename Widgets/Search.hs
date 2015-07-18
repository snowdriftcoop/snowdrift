module Widgets.Search where

import Import
import qualified Data.Text as T

data FilterClaimStatus = Claimed | Unclaimed | All deriving Eq

data SearchParameters = SearchParameters
    { claimed :: FilterClaimStatus
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
    (tagsRes, tagsView) <- mreq (tagWidget tags) "Tags" Nothing
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
              <p>
                <select>
                  <option>
                  <option>BEFORE
                  <option>AFTER
                  <option>BETWEEN
                <input type="date">
                <select>
                  <option>
                  <option>AND
                <input type="date">
              <p>tags:
              ^{fvInput tagsView}
            <div>
              <p>Sort: ^{fvInput sortView}
          |]

    return (searchRes, widget)

tagWidget :: [Text] -> Field Handler [(Text, Text)]
tagWidget tags = Field
    { fieldParse = \tagSelectValues _ ->
          if (length tagSelectValues > 0) then
              return $ Right $ Just $ zip tags tagSelectValues
          else
              return $ Left "Error with tag selection: No input."
    , fieldView = \_ _ otherAttrs _ _ ->
                    [whamlet|
                        $forall tag <- tags
                          <div .row>
                            <div .col-md-2>
                                <input type=radio name=#{tag} *{otherAttrs}>include
                            <div .col-md-2>
                                <input type=radio name=#{tag} *{otherAttrs}>exclude
                            <div .col-md-2>
                                <input type=radio name=#{tag} checked *{otherAttrs}>doesn't matter
                            <div .col-md-offset-2 .col-md-4>
                                #{tag}
                    |]
    , fieldEnctype = UrlEncoded
    }

searchFilterString :: SearchParameters -> Text
searchFilterString (SearchParameters Claimed tags _ ) = 
        T.append "CLAIMED AND " $ parseTags tags
searchFilterString (SearchParameters Unclaimed tags _ ) =
        T.append "UNCLAIMED AND " $ parseTags tags
searchFilterString (SearchParameters All tags _) =
        T.append T.empty $ parseTags tags

parseTags :: [(Text, Text)] -> Text
parseTags t = T.concat $ map fst t

searchSortString :: SearchParameters -> Text
searchSortString (SearchParameters _ _ sortString) = fromMaybe T.empty sortString
