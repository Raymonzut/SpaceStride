module LibHighScoreBoard where

import Model (HighScoreBoard)

import qualified Data.Map as Map
import Data.List
import Data.Maybe
import Data.List.Split
import System.Directory

hsSize :: Int
hsSize = 5


emptyScoreBoard = replicate hsSize ("...", 0)

updateHighScoreBoard :: Int -> String -> IO HighScoreBoard
updateHighScoreBoard pScore pName = do fileExist <- doesFileExist "game.log"
                                       if not fileExist
                                       then return emptyScoreBoard
                                       else do content <- readFile "game.log"
                                               return . update $ parse content
  where update hsBoard = take hsSize . sortBy cmpScores $ (pName, pScore) : hsBoard
        cmpScores x y = compare (snd y) (snd x)

parse :: String -> HighScoreBoard
parse blob = maybe emptyScoreBoard parseRows $ Map.lookup "highscore" parsedFile
  where parsedFile = Map.fromList (map (tuple2 . splitOn ":") $ lines blob)
        parseRows :: String -> HighScoreBoard
        parseRows blob = map (\xs -> (head xs, (read . head $ tail xs) :: Int)) . chunksOf 2 $ splitOn ";" blob

tuple2 :: [a] -> (a, a)
tuple2 (x : y : _) = (x, y)
