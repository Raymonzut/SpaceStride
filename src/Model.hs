{-# LANGUAGE TemplateHaskell #-}

module Model where

import Control.Lens
import Data.Map (Map, findWithDefault)
import GHC.Float

import Graphics.Gloss

data GameState = Playing {
                   _player :: Player
                 , _elapsedTime :: Float
                 }

data Player = Player {
                _moveDirection :: Direction
             -- Origin is center-bottom
              , _relPos :: Point
              , _size :: Point
              , _sprite :: Picture
              }

data Direction = North
               | East
               | South
               | West
               | Center

makeLenses ''GameState
makeLenses ''Player

initialState :: Map String Picture -> GameState
initialState assets = Playing (Player Center (0, 0) (100, 100) spaceshipSprite) 0
  where spaceshipSprite = findWithDefault noSprite "Spaceship" assets
        noSprite = undefined

secsPerUpdate :: Float
secsPerUpdate = 1 / ups
  where ups = int2Float fps

fps :: Int
fps = 60
