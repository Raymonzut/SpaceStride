module View where

import Graphics.Gloss
import Graphics.Gloss.Data.Point

import Model

screenSize :: (Int, Int)
screenSize = (240, 320)

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure _ = blank
