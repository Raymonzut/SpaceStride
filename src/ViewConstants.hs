module ViewConstants where


import Control.Lens
import GHC.Float
import Graphics.Gloss

screenSize :: (Int, Int)
screenSize = (240, 320)

screenSizeF :: (Float, Float)
screenSizeF = bimap int2Float int2Float screenSize

halfWidthOf, halfHeightOf :: (Int, Int) -> Float
halfWidthOf = int2Float . (`div` 2) . fst
halfHeightOf = int2Float . (`div` 2) . snd

screenBoundsH :: Point
screenBoundsH = ( -halfWidthOf screenSize + screenMargin
                ,  halfWidthOf screenSize - screenMargin)

screenMargin :: Float
screenMargin = 10
