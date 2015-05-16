module Widgets.Tag where

import Import

import Data.List (maximumBy)
import Data.Bits

import Model.Tag
import Text.Printf

import Model.Settings

pickForegroundColor :: Int -> Int
pickForegroundColor bg = maximumBy (compare `on` \a -> colorDiff a bg) [0x111111, 0xeeeeee]
  where
    colorDiff a b = sum $ map abs $ zipWith (-) (bytelist a) (bytelist b)
    bytelist a = map ((.&. 255) . shiftR a) [0, 8, 16]

{- We've been playing around with voting on tags and didn't finalize
an agreement about how to calculate voting, so that feature is currently
not presented on the live site. I (Aaron) am favoring the same range-voting
we propose for the co-op where there's a 6-point scale, but that's not
what the code has currently.
-}

tagWidget :: AnnotatedTag -> Widget
tagWidget t = do
    maybe_user_id <- handlerToWidget maybeAuthId

    show_tag_voting <- userSettingsShowTagVotes <$> handlerToWidget getUserSettings

    let my_tag = any ((== maybe_user_id) . Just . entityKey . fst) $ annotTagUserVotes t

    let maybe_user_score = maybe_user_id >>= annotTagUserScore t

    let bg :: String
        bg = printf "%06x" $ (\(Color c) -> c) $ annotTagColor t
        fg :: String
        fg = printf "%06x" $ pickForegroundColor $ (\(Color c) -> c) $ annotTagColor t

    toWidget [hamlet|
        <form .tag action=@{annotTagUrl t} style="background-color:##{bg};color:##{fg}" method=post>
            <small>
                #{annotTagName t}
                $if show_tag_voting
                    <input type=submit name=direction style="color:##{fg}" value=- .tag-input>
                    <span .tag-score>
                        $maybe user_score <- maybe_user_score
                            #{user_score}/#{annotTagScoreString t}
                        $nothing
                            #{annotTagScoreString t}
                    <input type=submit name=direction style="color:##{fg}" value=+ .tag-input>
                $else
                    $if my_tag
                        <input type=submit name=direction style="color:##{fg};" value=&times; .tag-input>
    |]

newTagWidget :: Route App -> Widget
newTagWidget route =
    toWidget [hamlet|
        <a href=@{route}>
            add tag
    |]

