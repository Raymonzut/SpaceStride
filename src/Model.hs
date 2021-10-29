module Model where

import Graphics.Gloss.Data.Point

data GameState = Playing {
                   player :: Player
                 , elapsedTime :: Float
                 }


data Player = Player {
             -- Origin is center-bottom
                relPos :: Point
              }

initialState :: GameState
initialState = Playing (Player (0, 0)) 0

secsPerUpdate :: Float
secsPerUpdate = 1 / ups
  where ups = 60

