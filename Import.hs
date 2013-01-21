{-# LANGUAGE CPP #-}

module Import
    ( module Import
    ) where

import           Prelude              as Import hiding (head, init, last,
                                                 readFile, tail, writeFile)
import           Yesod                as Import hiding (Route (..))
import           Yesod.Auth           as Import
import           Yesod.Markdown       as Import (markdownToHtml, Markdown, markdownField)

import           Control.Applicative  as Import (pure, (<$>), (<*>))
import           Data.Text            as Import (Text)

import           Foundation           as Import
import           Model                as Import
import           Settings             as Import
import           Settings.Development as Import
import           Settings.StaticFiles as Import

import           Data.Maybe           as Import (fromMaybe, listToMaybe, mapMaybe)

import           Data.Int             as Import (Int64)

import           Control.Monad        as Import (when)

import           Data.Time.Clock      as Import (UTCTime, diffUTCTime, getCurrentTime)
import           Data.Time.Units

#if __GLASGOW_HASKELL__ >= 704
import           Data.Monoid          as Import
                                                 (Monoid (mappend, mempty, mconcat),
                                                 (<>))
#else
import           Data.Monoid          as Import
                                                 (Monoid (mappend, mempty, mconcat))

infixr 5 <>
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
#endif


class Count a where
    getCount :: a -> Int64

data UserCount = UserCount Int64
instance Count UserCount where getCount (UserCount c) = c

data ShareCount = ShareCount Int64
instance Count ShareCount where getCount (ShareCount c) = c

-- from http://stackoverflow.com/questions/8066850/why-doesnt-haskells-prelude-read-return-a-maybe
readMaybe        :: (Read a) => String -> Maybe a
readMaybe s      =  case [x | (x,t) <- reads s, ("","") <- lex t] of
                         [x] -> Just x
                         _   -> Nothing


age :: UTCTime -> UTCTime -> String
age a b = let s = round $ toRational $ diffUTCTime a b
              f (t :: Second)
                 | t > convertUnit (1 :: Fortnight) = show $ (convertUnit t :: Fortnight)
                 | t > convertUnit (1 :: Week) = show $ (convertUnit t :: Week)
                 | t > convertUnit (1 :: Day) = show $ (convertUnit t :: Day)
                 | t > convertUnit (1 :: Hour) = show $ (convertUnit t :: Hour)
                 | otherwise = show $ (convertUnit t :: Minute)
           in f s

footnote :: Integer -> Widget
footnote note = [whamlet|$newline never
    <sup>
        <a name="fn_use#{show note}" href="#fn#{show note}">
            #{show note}
|]


footnote_anchor :: String -> Widget
footnote_anchor labels = 
    case words labels of
        (first_label : remaining_labels) ->
            [whamlet|$newline never
                [
                    <a .footnote_anchor name="fn#{first_label}" href="#fn_use#{first_label}">
                        #{first_label}

                    $forall label <- remaining_labels
                        ,
                        <a .footnote_anchor name="fn#{label}" href="#fn_use#{label}">
                            #{label}
                ]
            |]

        _ -> error "empty footnote anchor"

toc_entry :: String -> String -> Widget
toc_entry tag title =
    [whamlet|$newline never
        <li .toc_li>
            <a .toc_entry name="toc_entry#{tag}" href="#toc_target#{tag}">
                #{title}
    |]


toc_target :: String -> String -> Widget
toc_target tag title =
    [whamlet|$newline never
        <span .title>
            #{title}
        <sup>
            <a .toc_target name="toc_target#{tag}" href="#toc_entry#{tag}" title="Back to Table Of Contents">
                ^
    |]
