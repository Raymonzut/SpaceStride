module View where


import Graphics.Gloss

import Model
import ViewConstants

import Control.Lens

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate = Pictures ([
                    background
                  , playerPosT gstate (gstate ^. player . sprite)
                  ] ++ enemies')
  where playerSize = gstate ^. player . size
        background = Color (greyN 0.1) $ uncurry rectangleSolid screenSizeF
        enemies' = [ uncurry Translate enemyPos . Color black $ uncurry rectangleSolid enemySize
                   | enemyPos <- enemyPositions , enemySize <- sizes]

        enemyPositions :: [Point]
        enemyPositions = map (getMoveableScreenPos gstate) ((gstate ^. enemies) ^.. folded .re _Enemy)
        sizes = [(50, 50)]

playerPosT :: GameState -> Picture -> Picture
playerPosT gstate = Translate playerPosX playerPosY
  where playerPosX = fst $ gstate ^. player . relPos
        playerPosY = 0.5 * gstate ^. player . size . _2 - halfHeightOf screenSize + screenMargin
