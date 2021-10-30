{-# LANGUAGE TemplateHaskell #-}

module Model where

import Control.Lens
import Graphics.Gloss.Data.Point

data GameState = Playing {
                   _player :: Player
                 , _elapsedTime :: Float
                 }

data Player = Player {
             -- Origin is center-bottom
                _relPos :: Point
              }

data Direction = North
               | East
               | South
               | West
               | Center

makeLenses ''GameState
makeLenses ''Player

initialState :: GameState
initialState = Playing (Player (0, 0)) 0

secsPerUpdate :: Float
secsPerUpdate = 1 / ups
  where ups = 60

