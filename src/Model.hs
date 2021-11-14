{-# LANGUAGE TemplateHaskell #-}

module Model where

import Prelude hiding (negate)

import Control.Lens
import Data.Map (Map)
import GHC.Float

import Graphics.Gloss

data GameState = Playing { _playingGame :: PlayingState }
               | Paused { _pausedGame :: PlayingState }
               | GameOverTypeName { _score :: Int, _playerName :: String }
               | GameOverShowScores { _score :: Int
                                    , _playerName :: String
                                    , _highscoreBoard :: HighScoreBoard
                                    }

data PlayingState = PlayingState {
                   _assets :: Map String Picture
                 , _player :: PlayerData
                 , _enemies :: [EnemyData]
                 , _enemyKillCount :: Float
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
                }

data EnemyData = EnemyData {
                 _worldPos :: Point
               }

data Direction = North
               | East
               | South
               | West
               | Center

type HighScoreBoard = [(String, Int)]

makePrisms ''Moveable
makeLenses ''PlayerData
makeLenses ''EnemyData
makeLenses ''GameState
makeLenses ''PlayingState

getMoveableScreenPos :: PlayingState -> Moveable ->  Point
getMoveableScreenPos _ m
  | Just pd <- m ^? _Player = pd ^. relPos
  | Just ed <- m ^? _Enemy  = ed ^. worldPos

getScore :: PlayingState -> Int
getScore pstate = floor $ pstate ^. worldScroll
                + pstate ^. enemyKillCount

initialState :: Map String Picture -> GameState
initialState assets = Playing $ PlayingState assets (PlayerData Center (0, 0) (100, 100)) [] 0 0 0 0

secsPerUpdate :: Float
secsPerUpdate = 1 / ups
  where ups = int2Float fps

fps :: Int
fps = 60
