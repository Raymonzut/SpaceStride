module View where

import GHC.Float
import Graphics.Gloss
import Graphics.Gloss.Data.Point

import Model

screenSize :: (Int, Int)
screenSize = (240, 320)

halfWidthOf, halfHeightOf :: (Int, Int) -> Float
halfWidthOf = int2Float . (`div` 2) . fst
halfHeightOf = int2Float . (`div` 2) . snd

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure _ = blank
