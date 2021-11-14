{-# LANGUAGE FlexibleContexts #-}

module Controller where

import Model
import ViewConstants

import LibAssets (playerAnimationFrameCount)
import LibHighScoreBoard

import Control.Lens
import GHC.Float
import Graphics.Gloss.Interface.IO.Game
import qualified Graphics.Gloss.Data.Point.Arithmetic as Point ((-))
import System.Random

type GameStateT = GameState -> GameState
type PlayingStateT = PlayingState -> PlayingState

step :: Float -> GameState -> IO GameState
step secs (Playing pstate)
  | delta > secsPerUpdate
  = do randomNumber <- randomIO :: IO Int
       return $ (Playing $ pstate
         & seed %~ const randomNumber
         & movePlayer delta
         & moveEnemies delta
         & scrollBackground delta
         & attemptEnemySpawn
         & elapsedTime %~ const 0
        )& collisionCheck
  | otherwise
  = return . Playing $ pstate
    & elapsedTime %~ (+secs)
  where delta = pstate ^. elapsedTime + secs
step secs (PlayerDead pstate cnt) | cnt == mx = return $ GameOverTypeName (getScore pstate) ""
                                  | otherwise = return $ PlayerDead (pstate & elapsedTime %~ (+secs)) (float2Int ((pstate ^. elapsedTime) / 0.2))
  where mx = playerAnimationFrameCount
step _ gstate = return gstate

input :: Event -> GameState -> IO GameState
input e gstate@(GameOverTypeName _ __) = typeName e gstate
input e gstate = return (inputKey e gstate)

typeName :: Event -> GameState -> IO GameState
typeName (EventKey (Char key) Down _ _) gstate@(GameOverTypeName _ __) = return $ gstate & playerName %~ (++ [key])
typeName (EventKey (SpecialKey KeyEnter) Down _ _) (GameOverTypeName pScore pName)
  = do hsBoard <- updateHighScoreBoard pScore pName
       return $ GameOverShowScores pScore pName hsBoard
typeName _ gstate = return gstate

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char 'p') Down _ _) (Playing pstate) = Paused pstate
inputKey (EventKey (Char 'p') Down _ _) (Paused  pstate) = Playing pstate
inputKey (EventKey (Char 'q') Down _ _) (Playing pstate) = GameOverTypeName (getScore pstate) ""
inputKey (EventKey (Char 'q') Down _ _) (Paused  pstate) = GameOverTypeName (getScore pstate) ""
inputKey (EventKey (Char c) ks _ _)     (Playing pstate) = Playing $ pstate
  & (player . moveDirection %~ updatePlayerDirection c ks)
inputKey _ gstate = gstate

updatePlayerDirection :: Char -> KeyState -> Direction -> Direction
-- Basic single key movement
updatePlayerDirection 'a' Up West = Center
updatePlayerDirection 'a' Down Center = West
updatePlayerDirection 'd' Up East = Center
updatePlayerDirection 'd' Down Center = East
-- Advanced multi key movement
--- Rebound a -> d
updatePlayerDirection 'a' Up Center = East
updatePlayerDirection 'd' Down West = Center
--- Rebound d -> a
updatePlayerDirection 'd' Up Center = West
updatePlayerDirection 'a' Down East = Center
-- Otherwise
updatePlayerDirection _   _ dir = dir

movePlayer :: Float -> PlayingStateT
movePlayer delta pstate = pstate & player . relPos %~ newPos dir
  where dir = pstate ^. player . moveDirection
        newPos West pos = pos & _1 %~ borderedH (+(-spd))
        newPos East pos = pos & _1 %~ borderedH (+spd)
        newPos _    pos = pos
        borderedH = withinScreenBordersH $ pstate ^. player . size
        spd = 120 * delta

moveEnemies :: Float -> PlayingStateT
moveEnemies delta pstate = pstate & enemies . each %~ incPos
  where incPos enemy = enemy & worldPos . _2 %~ (+(-((speed + pstate ^. worldScroll) * delta)))
        speed = 70

collisionCheck :: GameStateT
collisionCheck (Playing pstate)
  | null hasHit = Playing pstate
  | otherwise   = PlayerDead pstate 0
  where hasHit = filter (\(dx, dy) -> (dx * dx + dy * dy) < (playerSize2 + enemySize2)) distances
        distances = map (pstate ^. player . relPos Point.-) (pstate ^. enemies ^.. traverse . worldPos)
        playerSize2 = max (pstate ^. player . size) (pstate ^. player . size)
        enemySize2 = 25 * 25
collisionCheck gstate = gstate

scrollBackground :: Float -> PlayingStateT
scrollBackground delta pstate = pstate & worldScroll %~ (+delta)

attemptEnemySpawn :: PlayingStateT
attemptEnemySpawn pstate = pstate & enemies %~ addEnemy
  where willSpawn = (pstate ^. seed) `mod` 26 == 0
        addEnemy xs | willSpawn = EnemyData (x, halfHeightOf screenSize) : xs
                    | otherwise = xs
        x = int2Float (((pstate ^. seed) `div` 10) `mod` (screenSize ^. _1) - float2Int (halfWidthOf screenSize))

withinScreenBordersH :: Float -> (Float -> Float) -> Float -> Float
withinScreenBordersH sizeH f old = snd . head $ filter fst bounded
  where bounded = zipWith (\g bound -> (g bound, if g bound then bound else new)) limiters boundsH
        boundsH = [fst screenBoundsH + sizeH * 0.5, snd screenBoundsH - sizeH * 0.5, new]
        limiters = [(new <), (new >), const True]
        new = f old
