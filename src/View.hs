module View where

import Model
import ViewConstants

import LibAssets (lookupSprite)

import Control.Lens
import Graphics.Gloss

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure (Paused pstate)                           = Pictures [ viewPure (Playing pstate)
                                                              , pauseLayer
                                                              ]
viewPure (Playing pstate)                          = Pictures [ gameView pstate
                                                              , playerPosT pstate (lookupSprite "Spaceship" (pstate ^. assets))
                                                              , topLayerScore pstate
                                                              ]
viewPure (PlayerDead pstate cnt)                   = Pictures [ gameView pstate
                                                              , topLayerScore pstate
                                                              , playerPosT pstate (lookupSprite ("Spaceship" ++ show cnt) (pstate ^. assets))
                                                              ]
viewPure (GameOverTypeName pScore pName)           = Pictures [ gameOverMessage
                                                              , namedScoreLayer pScore pName
                                                              ]
viewPure (GameOverShowScores pScore pName hsBoard) = Pictures [ gameOverMessage
                                                              , namedScoreLayer pScore pName
                                                              , highScoreBoard
                                                              ]
  where highScoreBoard = leftAligned . Pictures $ hsHeader : scoreRows
        hsHeader = textScaleMedium . Color white $ Text "Highscores"
        scoreRows = map (\(txt, offset) -> Translate 0 offset . textScaleMedium . Color white $ Text txt) lines
        lines = zip (map (\(pName, pScore) -> pName ++ " : " ++ show pScore) hsBoard) (map (*(-20)) [1..])

background :: Picture
background = Color (greyN 0.1) $ rectangle screenSizeF

namedScoreLayer :: Int -> String -> Picture
namedScoreLayer pScore pName = Translate 0 (0.5 * halfWidthOf screenSize) . leftAligned . Color white
                             $ Pictures [ textScaleMedium . Text $ "De score " ++ show pScore
                                        , Translate 0 (-20) . textScaleMedium . Text $ "Voor speler: " ++ pName
                                        ]

gameOverMessage :: Picture
gameOverMessage = topLayer 24 . Color white . textScaleLarge $ Text "Game Over!"

pauseLayer :: Picture
pauseLayer = Pictures [fade, pauseText]
  where fade = Color (makeColorI 50 50 50 100) $ rectangle screenSizeF
        pauseText = textScaleLarge $ Text "Paused"

topLayer :: Float -> Picture -> Picture
topLayer layerHeight = Translate (-halfWidthOf screenSize) (halfHeightOf screenSize - layerHeight)

leftAligned :: Picture -> Picture
leftAligned = Translate (-halfWidthOf screenSize + margin) 0
  where margin = 10

topLayerScore :: PlayingState -> Picture
topLayerScore pstate = topLayer layerHeight (Pictures [layerOutline, scoreLayer pstate])
  where layerOutline = Translate (halfWidthOf screenSize) (layerHeight / 2)
                     . Color (greyN 0.25) $ rectangle (fst screenSizeF, layerHeight)
        layerHeight = 24

scoreLayer :: PlayingState -> Picture
scoreLayer pstate = Translate 0 1 . textScaleLarge . Text $ "Score: " ++ show (getScore pstate)

textScaleMedium, textScaleLarge :: Picture -> Picture
textScaleMedium = scale 0.12 0.12
textScaleLarge = scale 0.2 0.2

gameView :: PlayingState -> Picture
gameView pstate = Pictures $ background : enemies'
  where enemies'   = [ uncurry Translate enemyPos $ lookupSprite "Rock" (pstate ^. assets)
                     | enemyPos <- enemyPositions]
        enemyPositions = map (getMoveableScreenPos pstate) ((pstate ^. enemies) ^.. folded .re _Enemy)

playerPosT :: PlayingState -> Picture -> Picture
playerPosT pstate = uncurry Translate $ pstate ^. player . relPos

rectangle :: Point -> Picture
rectangle = uncurry rectangleSolid
