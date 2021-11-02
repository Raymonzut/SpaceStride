module Main where

import Controller
import Model
import View

import Data.Map (fromList)
import Data.Maybe

import Graphics.Gloss.Juicy
import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = do assets <- loadAssets

          let assetsMap = fromList $ zip images assets

          playIO (InWindow "SpaceStride" screenSize (0, 0))
                 black                -- Background color
                 fps                  -- Frames per second
                (initialState assetsMap) -- Initial state
                 view                 -- View function
                 input                -- Event function
                 step                 -- Step function

images :: [String]
images = ["Spaceship"]

loadAssets :: IO [Picture]
loadAssets = mapM (unwrap . load) images
  where unwrap = fmap fromJust
        load image = loadJuicyPNG ("./assets/" ++ image ++ ".png")
