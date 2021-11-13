module View where


import Graphics.Gloss

import Model
import ViewConstants

import Control.Lens

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure (Paused pstate)  = Pictures [ viewPure (Playing pstate)
                                     , pauseLayer
                                     ]
viewPure (Playing pstate) = Pictures [ gameView pstate
                                     , topLayer pstate
                                     ]

pauseLayer :: Picture
pauseLayer = Pictures [fade, pauseText]
  where fade = Color (makeColorI 50 50 50 100) $ rectangleSolid 1000 1000
        pauseText = textScale $ Text "Paused"

topLayer :: PlayingState -> Picture
topLayer pstate = Translate (-halfWidthOf screenSize) (halfHeightOf screenSize - layerHeight)
                $ Pictures [layerOutline, scoreLayer pstate]
  where layerHeight = 24
        layerOutline = Translate (halfWidthOf screenSize) (layerHeight / 2)
                     . Color (greyN 0.25) $ rectangleSolid (fst screenSizeF) layerHeight

scoreLayer :: PlayingState -> Picture
scoreLayer pstate = Translate 0 1 . textScale . Text $ "Score: " ++ show (getScore pstate)

textScale :: Picture -> Picture
textScale = scale 0.2 0.2

gameView :: PlayingState -> Picture
gameView pstate = Pictures ([
                    background
                  , playerPosT pstate (pstate ^. player . sprite)
                  ] ++ enemies')
  where playerSize = pstate ^. player . size
        background = Color (greyN 0.1) $ uncurry rectangleSolid screenSizeF
        enemies' = [ uncurry Translate enemyPos . Color black $ uncurry rectangleSolid enemySize
                   | enemyPos <- enemyPositions , enemySize <- sizes]

        enemyPositions :: [Point]
        enemyPositions = map (getMoveableScreenPos pstate) ((pstate ^. enemies) ^.. folded .re _Enemy)
        sizes = [(50, 50)]

playerPosT :: PlayingState -> Picture -> Picture
playerPosT pstate = Translate playerPosX playerPosY
  where playerPosX = fst $ pstate ^. player . relPos
        playerPosY = 0.5 * pstate ^. player . size . _2 - halfHeightOf screenSize + screenMargin
