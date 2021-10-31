{-# LANGUAGE FlexibleContexts #-}

module Controller where

import Model
import View(screenBoundsH)

import Control.Lens
import Graphics.Gloss.Interface.IO.Game

type GameStateT = GameState -> GameState

step :: Float -> GameState -> IO GameState
step secs gstate
  | delta > secsPerUpdate
  = return $ gstate
    & movePlayer delta
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

withinScreenBordersH :: Float -> (Float -> Float) -> Float -> Float
withinScreenBordersH sizeH f old = snd . head $ filter fst bounded
  where bounded = zipWith (\g bound -> (g bound, if g bound then bound else new)) limiters boundsH
        boundsH = [fst screenBoundsH + sizeH * 0.5, snd screenBoundsH - sizeH * 0.5, new]
        limiters = [(new <), (new >), const True]
        new = f old
