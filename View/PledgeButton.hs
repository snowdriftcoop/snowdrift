{-# LANGUAGE NoMonomorphismRestriction #-}

module View.PledgeButton (
	overlayImage,
	fillInPledgeCount,
	blankPledgeButton,
) where

import Import hiding ((<>))
import Diagrams.Prelude
import Diagrams.Backend.Rasterific
import Codec.Picture.Png
import Codec.Picture.Types
import Data.FileEmbed
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

fillInPledgeCount :: Integer -> Diagram B R2
fillInPledgeCount num = positiontext (text str # styled) <> boundingbox
  where
	str = addCommas (show num)
	styled t = t # fc blue # fontSize (Local 16)
	-- Magic numbers position the text in the center of the box
	-- in the pledge button where it's supposed to go.
	positiontext = translateX (-100) . translateY 12
	-- invisible bounding box for text
	boundingbox = rect 50 20 # lw none

addCommas :: String -> String
addCommas = reverse . go . reverse
  where
	go (c1:c2:c3:rest) | not (null rest) = c1:c2:c3:',':addCommas rest
	go s = s

renderByteStringPng :: Int -> Diagram B R2 -> L.ByteString
renderByteStringPng w = encodePng . renderDia Rasterific opts
  where
	opts = RasterificOptions (Width (fromIntegral w))

-- | Use an image as the base of a diagram, drawing over it.
--
-- Returns a png in a ByteString that has the same size as the original
-- image.
overlayImage :: DImage Embedded -> Diagram B R2 -> L.ByteString
overlayImage img@(DImage _ w _ _) overlay = renderByteStringPng w dia
  where
	dia = overlay <> image img # sized Absolute

blankPledgeButton :: DImage Embedded
blankPledgeButton = DImage (ImageRaster img) w h mempty
  where
	img = either (error "bad static/img/pledge-button.png") id $
		decodePng blankPledgeButtonPng
	w = dynamicMap imageWidth img
	h = dynamicMap imageHeight img

blankPledgeButtonPng :: B.ByteString
blankPledgeButtonPng = $(embedFile "static/img/pledge-button.png")
