module View where

import Control.Lens
import GHC.Float
import Graphics.Gloss

import Model

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

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate = Pictures [
                    background
                  , playerPosT gstate . Color white $ uncurry rectangleSolid playerSize
                  ]
  where playerSize = gstate ^. player . size
        background = Color (greyN 0.1) $ uncurry rectangleSolid screenSizeF

playerPosT :: GameState -> Picture -> Picture
playerPosT gstate = Translate playerPosX playerPosY
  where playerPosX = fst $ gstate ^. player . relPos
        playerPosY = 0.5 * gstate ^. player . size . _2 - halfHeightOf screenSize + screenMargin
