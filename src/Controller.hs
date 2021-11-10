{-# LANGUAGE FlexibleContexts #-}

module Controller where

import Model
import ViewConstants(screenBoundsH)

import Control.Lens
import Graphics.Gloss.Interface.IO.Game
import System.Random

type GameStateT = GameState -> GameState

step :: Float -> GameState -> IO GameState
step secs gstate
  | delta > secsPerUpdate
  = do randomNumber <- randomIO :: IO Int
       return $ gstate
         & seed %~ const randomNumber
         & movePlayer delta
         & moveEnemies delta
         -- & scrollBackground delta
         & attemptEnemySpawn
         & elapsedTime %~ const 0
  | otherwise
  = return $ gstate
    & elapsedTime %~ (+secs)
  where delta = gstate ^. elapsedTime + secs

input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameStateT
inputKey (EventKey (Char c) ks _ _) gstate = gstate
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

movePlayer :: Float -> GameStateT
movePlayer delta gstate = gstate & player . relPos %~ newPos dir
  where dir = gstate ^. player . moveDirection
        newPos West pos = pos & _1 %~ borderedH (+(-spd))
        newPos East pos = pos & _1 %~ borderedH (+spd)
        newPos _    pos = pos
        borderedH = withinScreenBordersH $ gstate ^. player . size . _1
        spd = 80 * delta

moveEnemies :: Float -> GameStateT
moveEnemies delta gstate = gstate & enemies . each %~ incPos
  where incPos enemy = enemy & worldPos . _2 %~ (+(-((speed + gstate ^. worldScroll) * delta)))
        speed = 10

scrollBackground :: Float -> GameStateT
scrollBackground delta gstate = undefined

attemptEnemySpawn :: GameStateT
attemptEnemySpawn gstate = gstate & enemies %~ addEnemy
  where willSpawn = (gstate ^. seed) `mod` 420 == 0
        addEnemy xs | willSpawn = EnemyData (0, -10) : xs
                    | otherwise = xs

withinScreenBordersH :: Float -> (Float -> Float) -> Float -> Float
withinScreenBordersH sizeH f old = snd . head $ filter fst bounded
  where bounded = zipWith (\g bound -> (g bound, if g bound then bound else new)) limiters boundsH
        boundsH = [fst screenBoundsH + sizeH * 0.5, snd screenBoundsH - sizeH * 0.5, new]
        limiters = [(new <), (new >), const True]
        new = f old
