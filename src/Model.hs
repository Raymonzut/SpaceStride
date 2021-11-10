{-# LANGUAGE TemplateHaskell #-}

module Model where

import Prelude hiding ((+), negate)

import Control.Lens
import Data.Map (Map, findWithDefault)
import GHC.Float

import Graphics.Gloss

data GameState = Playing { _playingGame :: PlayingState }
               | Paused { _pausedGame :: PlayingState }

data PlayingState = PlayingState {
                   _player :: PlayerData
                 , _enemies :: [EnemyData]
                 , _worldScroll :: Float
                 , _elapsedTime :: Float
                 , _seed :: Int
                 }

data Moveable = Player PlayerData
              | Enemy  EnemyData

data PlayerData = PlayerData {
                  _moveDirection :: Direction
               -- Origin is center-bottom
                , _relPos :: Point
                , _size :: Point
                , _sprite :: Picture
                }

data EnemyData = EnemyData {
                 _worldPos :: Point
               }

data Direction = North
               | East
               | South
               | West
               | Center

makePrisms ''Moveable
makeLenses ''PlayerData
makeLenses ''EnemyData
makeLenses ''GameState
makeLenses ''PlayingState

getMoveableScreenPos :: PlayingState -> Moveable ->  Point
getMoveableScreenPos _ m
  | Just pd <- m ^? _Player = pd ^. relPos
  | Just ed <- m ^? _Enemy  = ed ^. worldPos


initialState :: Map String Picture -> GameState
initialState assets = Playing $ PlayingState (PlayerData Center (0, 0) (100, 100) spaceshipSprite) [] 0 0 0
  where spaceshipSprite = findWithDefault noSprite "Spaceship" assets
        noSprite = undefined

secsPerUpdate :: Float
secsPerUpdate = 1 / ups
  where ups = int2Float fps

fps :: Int
fps = 60
