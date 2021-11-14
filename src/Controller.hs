{-# LANGUAGE FlexibleContexts #-}

module Controller where

import Model
import ViewConstants(screenBoundsH)
import LibHighScoreBoard

import Control.Lens
import Graphics.Gloss.Interface.IO.Game
import System.Random

type PlayingStateT = PlayingState -> PlayingState

step :: Float -> GameState -> IO GameState
step secs (Playing pstate)
  | delta > secsPerUpdate
  = do randomNumber <- randomIO :: IO Int
       return . Playing $ pstate
         & seed %~ const randomNumber
         & movePlayer delta
         & moveEnemies delta
         & scrollBackground delta
         & attemptEnemySpawn
         & elapsedTime %~ const 0
  | otherwise
  = return . Playing $ pstate
    & elapsedTime %~ (+secs)
  where delta = pstate ^. elapsedTime + secs
step _ gstate = return gstate

input :: Event -> GameState -> IO GameState
input e gstate@(GameOverTypeName _ __) = typeName e gstate
input e gstate = return (inputKey e gstate)

typeName :: Event -> GameState -> IO GameState
typeName (EventKey (Char key) Down _ _) gstate@(GameOverTypeName _ __) = return $ gstate & playerName %~ (++ [key])
typeName (EventKey (SpecialKey KeyEnter) Down _ _) (GameOverTypeName pScore pName) = do hsBoard <- updateHighScoreBoard pScore pName
                                                                                        return $ GameOverShowScores pScore pName hsBoard
typeName _ gstate = return gstate

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char 'p') Down _ _) (Playing pstate) = Paused pstate
inputKey (EventKey (Char 'p') Down _ _) (Paused pstate) = Playing pstate

inputKey (EventKey (Char 'q') Down _ _) (Playing pstate) = GameOverTypeName (getScore pstate) ""
inputKey (EventKey (Char 'q') Down _ _) (Paused pstate) = GameOverTypeName (getScore pstate) ""

inputKey (EventKey (Char c) ks _ _) (Playing pstate) = Playing $ pstate
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
        borderedH = withinScreenBordersH $ pstate ^. player . size . _1
        spd = 80 * delta

moveEnemies :: Float -> PlayingStateT
moveEnemies delta pstate = pstate & enemies . each %~ incPos
  where incPos enemy = enemy & worldPos . _2 %~ (+(-((speed + pstate ^. worldScroll) * delta)))
        speed = 10

scrollBackground :: Float -> PlayingStateT
scrollBackground delta pstate = pstate & worldScroll %~ (+delta)

attemptEnemySpawn :: PlayingStateT
attemptEnemySpawn pstate = pstate & enemies %~ addEnemy
  where willSpawn = (pstate ^. seed) `mod` 420 == 0
        addEnemy xs | willSpawn = EnemyData (0, -10) : xs
                    | otherwise = xs

withinScreenBordersH :: Float -> (Float -> Float) -> Float -> Float
withinScreenBordersH sizeH f old = snd . head $ filter fst bounded
  where bounded = zipWith (\g bound -> (g bound, if g bound then bound else new)) limiters boundsH
        boundsH = [fst screenBoundsH + sizeH * 0.5, snd screenBoundsH - sizeH * 0.5, new]
        limiters = [(new <), (new >), const True]
        new = f old
