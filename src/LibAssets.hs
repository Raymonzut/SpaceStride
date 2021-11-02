module LibAssets where

import Data.Maybe

import Data.Map (Map, fromList)

import Graphics.Gloss
import Graphics.Gloss.Juicy


loadImagesAsMap :: [String] -> IO (Map String Picture)
loadImagesAsMap images = do assets <- loadAssets images
                            return . fromList $ zip images assets

loadAssets :: [String] -> IO [Picture]
loadAssets = mapM (unwrap . load)
  where unwrap = fmap fromJust
        load image = loadJuicyPNG ("./assets/" ++ image ++ ".png")
