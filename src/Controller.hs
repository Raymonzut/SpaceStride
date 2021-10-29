module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random


step :: Float -> GameState -> IO GameState
step secs gstate
  | elapsedTime gstate + secs > secsPerUpdate
  = return initialState
  | otherwise
  = return $ gstate { elapsedTime = elapsedTime gstate + secs }

input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey _ gstate = gstate
