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
                                     , topLayerScore pstate
                                     ]

viewPure (GameOverTypeName pScore pName) = Pictures [ topLayer 24 gameOverMessage
                                                    , namedScoreLayer pScore pName
                                                    ]
viewPure (GameOverShowScores pScore pName hsBoard) = Pictures [ topLayer 24 gameOverMessage
                                                              , namedScoreLayer pScore pName
                                                              , highScoreBoard
                                                              ]
  where highScoreBoard = leftAligned . Pictures $ hsHeader : scoreRows
        hsHeader = textScaleMedium . Color white $ Text "Highscores"
        scoreRows = map (\(txt, offset) -> Translate 0 offset . textScaleMedium . Color white $ Text txt) lines
        lines = zip (map (\(player, score) -> player ++ " : " ++ show score) hsBoard) (map (*(-20)) [1..])

namedScoreLayer :: Int -> String -> Picture
namedScoreLayer pScore pName = Translate 0 (0.5 * halfWidthOf screenSize) . leftAligned . Color white
                             $ Pictures [ textScaleMedium . Text $ "De score " ++ show pScore
                                        , Translate 0 (-20) . textScaleMedium . Text $ "Voor speler: " ++ pName
                                        ]

gameOverMessage :: Picture
gameOverMessage = Color white . textScaleLarge $ Text "Game Over!"

pauseLayer :: Picture
pauseLayer = Pictures [fade, pauseText]
  where fade = Color (makeColorI 50 50 50 100) $ rectangleSolid 1000 1000
        pauseText = textScaleLarge $ Text "Paused"

topLayer :: Float -> Picture -> Picture
topLayer layerHeight = Translate (-halfWidthOf screenSize) (halfHeightOf screenSize - layerHeight)

leftAligned :: Picture -> Picture
leftAligned = Translate (-halfWidthOf screenSize + margin) 0
  where margin = 10

topLayerScore :: PlayingState -> Picture
topLayerScore pstate = topLayer layerHeight (Pictures [layerOutline, scoreLayer pstate])
  where layerOutline = Translate (halfWidthOf screenSize) (layerHeight / 2)
                     . Color (greyN 0.25) $ rectangleSolid (fst screenSizeF) layerHeight
        layerHeight = 24

scoreLayer :: PlayingState -> Picture
scoreLayer pstate = Translate 0 1 . textScaleLarge . Text $ "Score: " ++ show (getScore pstate)

textScaleMedium, textScaleLarge :: Picture -> Picture
textScaleMedium = scale 0.12 0.12
textScaleLarge = scale 0.2 0.2

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
