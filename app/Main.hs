module Main where

import Controller
import Model
import View
import ViewConstants

import LibAssets

import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = do assetsMap <- loadImagesAsMap images

          playIO (InWindow "SpaceStride" screenSize (0, 0))
                 black                -- Background color
                 fps                  -- Frames per second
                (initialState assetsMap) -- Initial state
                 view                 -- View function
                 input                -- Event function
                 step                 -- Step function

images :: [String]
images = [ "NoSprite"
         , "Spaceship"
         , "Rock"
         ] ++ ["Spaceship" ++ show frame | frame <- [0..playerAnimationFrameCount] :: [Int]]
