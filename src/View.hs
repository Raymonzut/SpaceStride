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

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate = Pictures [
                    playerPosT gstate . Color white $ rectangleSolid 100 100
                  ]

playerPosT :: GameState -> Picture -> Picture
playerPosT gstate = Translate playerPosX playerPosY
  where playerPosX = fst $ gstate ^. player . relPos
        playerPosY = 0
