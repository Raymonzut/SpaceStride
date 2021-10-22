module Model where

data GameState = GameState {
                   infoToShow  :: InfoToShow
                 , elapsedTime :: Float
                 }

data InfoToShow = ShowNothing

initialState :: GameState
initialState = GameState ShowNothing 0

secsPerUpdate :: Float
secsPerUpdate = 1 / ups
  where ups = 60

