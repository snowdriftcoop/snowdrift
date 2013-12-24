
module Widgets.Tag where

import Import

import Data.List (maximumBy)
import Data.Bits

import Model.AnnotatedTag
import Text.Printf

import Model.Settings

pickForegroundColor :: Int -> Int
pickForegroundColor bg = maximumBy (compare `on` \ a -> colorDiff a bg) [0x111111, 0xeeeeee]
  where
    colorDiff a b = sum $ map abs $ zipWith (-) (bytelist a) (bytelist b)
    bytelist a = map (.&. 255) . map (shiftR a) $ [0, 8, 16]

tagWidget :: AnnotatedTag -> Widget
tagWidget t = do
    maybe_user_id <- handlerToWidget maybeAuthId

    show_tag_voting <- userSettingsShowTagVotes <$> handlerToWidget getUserSettings

    let my_tag = any ((== maybe_user_id) . Just . entityKey . fst) $ atUserVotes t

    let maybe_user_score = maybe_user_id >>= atUserScore t

    let bg :: String
        bg = printf "%06x" $ (\ (Color c) -> c) $ atColor t
        fg :: String
        fg = printf "%06x" $ pickForegroundColor $ (\ (Color c) -> c) $ atColor t

    toWidget [hamlet|
        <form .tag action=@{atUrl t} style="background-color:##{bg};color:##{fg}" method=post>
            <small>
                #{atName t}
                $if show_tag_voting
                    <input type=submit name=direction style="color:##{fg}" value=- .tag-input>
                    $maybe user_score <- maybe_user_score
                        #{user_score}/#{atScoreString t}
                    $nothing
                        #{atScoreString t}
                    <input type=submit name=direction style="color:##{fg}" value=+ .tag-input>
                $else
                    $if my_tag
                        <input type=submit name=direction style="color:##{fg}" value=- .tag-input>
    |]

newTagWidget :: Route App -> Widget
newTagWidget route = do
    toWidget [hamlet|
        <a href=@{route}>
            add tag
    |]

