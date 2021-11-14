module LibAssets where

import Data.Maybe

import Data.Map (Map, findWithDefault, fromList)
import qualified Data.Map (lookup)

import Graphics.Gloss
import Graphics.Gloss.Juicy


loadImagesAsMap :: [String] -> IO (Map String Picture)
loadImagesAsMap images = do assets <- loadAssets images
                            return . fromList $ zip images assets

loadAssets :: [String] -> IO [Picture]
loadAssets = mapM (unwrapPictureWithDefault . loadSingle)

loadSingle :: String -> IO (Maybe Picture)
loadSingle = loadJuicyPNG . toAssetPath

unwrapPictureWithDefault :: IO (Maybe Picture) -> IO Picture
unwrapPictureWithDefault inpureUnsurePic = do maybeSprite <- inpureUnsurePic
                                              case maybeSprite of
                                                (Just p) -> return p
                                                Nothing -> defaultSprite
  where
        defaultSprite = fromJust <$> loadSingle "NoSprite"

toAssetPath :: String -> String
toAssetPath filename = "./assets/" ++ filename ++ ".png"

lookupSprite :: String -> Map String Picture -> Picture
lookupSprite name assets = findWithDefault defaultSprite name assets
  where defaultSprite = fromJust $ Data.Map.lookup "NoSprite" assets

playerAnimationFrameCount :: Int
playerAnimationFrameCount = 30
