module View where

import Control.Lens
import GHC.Float
import Graphics.Gloss

import Model

screenSize :: (Int, Int)
screenSize = (240, 320)

halfWidthOf, halfHeightOf :: (Int, Int) -> Float
halfWidthOf = int2Float . (`div` 2) . fst
halfHeightOf = int2Float . (`div` 2) . snd

screenBoundsH :: Point
screenBoundsH = ( -halfWidthOf screenSize
                ,  halfWidthOf screenSize)

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate = Pictures [
                    playerPosT gstate . Color white $ uncurry rectangleSolid playerSize
                  ]
  where playerSize = gstate ^. player . size

playerPosT :: GameState -> Picture -> Picture
playerPosT gstate = Translate playerPosX playerPosY
  where playerPosX = fst $ gstate ^. player . relPos
        playerPosY = 0
