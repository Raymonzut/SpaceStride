{-# LANGUAGE FlexibleContexts #-}

module Controller where

import Model
import View(screenBoundsH)

import Control.Lens
import Graphics.Gloss.Interface.IO.Game

type GameStateT = GameState -> GameState

step :: Float -> GameState -> IO GameState
step secs gstate
  | gstate ^. elapsedTime + secs > secsPerUpdate
  = return gstate
  | otherwise
  = return $ gstate & elapsedTime %~ (+secs)

input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameStateT
inputKey (EventKey (Char c) _ _ _) gstate = gstate
  & (movePlayer $ toDirection c)
inputKey _ gstate = gstate

toDirection :: Char -> Direction
toDirection 'a' = West
toDirection 'd' = East
toDirection _ = Center

movePlayer :: Direction -> GameStateT
movePlayer dir gstate = gstate & player . relPos %~ newPos dir
  where newPos West pos = pos & _1 %~ borderedH (+(-spd))
        newPos East pos = pos & _1 %~ borderedH (+spd)
        newPos _    pos = pos
        borderedH = withinScreenBordersH $ gstate ^. player . size . _1
        spd = 10

withinScreenBordersH :: Float -> (Float -> Float) -> Float -> Float
withinScreenBordersH sizeH f old = snd . head $ filter fst bounded
  where bounded = zipWith (\g bound -> (g bound, if g bound then bound else new)) limiters boundsH
        boundsH = [fst screenBoundsH + sizeH * 0.5, snd screenBoundsH - sizeH * 0.5, new]
        limiters = [(new <), (new >), const True]
        new = f old
