module Model where

data GameState = Playing {
                   elapsedTime :: Float
                 }

initialState :: GameState
initialState = Playing 0

secsPerUpdate :: Float
secsPerUpdate = 1 / ups
  where ups = 60

