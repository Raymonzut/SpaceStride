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
  & (movePlayer $ toDirection c)
inputKey _ gstate = gstate

toDirection :: Char -> Direction
toDirection 'a' = West
toDirection 'd' = East
toDirection _ = Center

movePlayer :: Direction -> GameStateT
movePlayer dir gstate = gstate & player . relPos %~ newPos dir
  where newPos West pos = pos & _1 -~ spd
        newPos East pos = pos & _1 +~ spd
        newPos _    pos = pos
        spd = 10
