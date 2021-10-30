module Controller where

import Model

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
inputKey _ gstate = gstate
