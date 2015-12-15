{-# LANGUAGE LambdaCase #-}

module Css
        ( Color(..)
        , Breakpoint(..)
        ) where

import Data.Monoid ((<>))

import Text.Cassius hiding (Color)

--
-- Colors
--

data Color
        = DarkBlue
        | BrightBlue
        | BrightBlueText
        | White
        | Green
        | GreenShade
        | Gold
        | Red

instance ToCss Color where
    toCss = \case
        DarkBlue -> "#13628E"
        BrightBlue -> "#C5F1FD"
        BrightBlueText -> "#47CFEC"
        White -> "#FFFFFF"
        Green -> "#4EBF7A"
        GreenShade -> "#44A76B"
        Gold -> "#F9FF68"
        Red -> "#D66A6A"

--
-- Breakpoints
--

data Breakpoint
        = Break1
        | Break2
        | Break3
        | Break4
        | Break5
        | Break6

instance ToCss Breakpoint where
    toCss = \case
        Break1 -> hack "min-width: 301px"
        Break2 -> hack "min-width: 391px"
        Break3 -> hack "min-width: 571px"
        Break4 -> hack "min-width: 751px"
        Break5 -> hack "min-width: 841px"
        Break6 -> hack "min-width: 1111px"
      where
        hack s = "@media (" <> s <> ") {\n"
