module View where

import Control.Lens
import GHC.Float
import Graphics.Gloss
import qualified Graphics.Gloss.Data.Point.Arithmetic as Point ((-))

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
viewPure gstate = Pictures ([
                    background
                  , playerPosT gstate (gstate ^. player . sprite)
                  ] ++ enemies')
  where playerSize = gstate ^. player . size
        background = Color (greyN 0.1) $ uncurry rectangleSolid screenSizeF
        --enemies' = [Color black $ uncurry rectangleSolid pos | pos <- positions]
        enemies' = [ uncurry Translate (enemyPos Point.- (0, gstate ^. worldScroll)) . Color black $ uncurry rectangleSolid enemySize
                   | enemyPos <- enemyPositions , enemySize <- sizes]

        enemyPositions :: [Point]
        enemyPositions = gstate ^. enemies ^.. folded . worldPos
        sizes = [(50, 50)]

playerPosT :: GameState -> Picture -> Picture
playerPosT gstate = Translate playerPosX playerPosY
  where playerPosX = fst $ gstate ^. player . relPos
        playerPosY = 0.5 * gstate ^. player . size . _2 - halfHeightOf screenSize + screenMargin
