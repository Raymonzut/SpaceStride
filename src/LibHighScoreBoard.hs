module LibHighScoreBoard where

import Model (HighScoreBoard)

import Control.DeepSeq
import Control.Exception (evaluate)
import qualified Data.Map as Map
import Data.List
import Data.List.Split
import System.Directory
import System.IO


hsSize :: Int
hsSize = 5

logFilePath :: FilePath
logFilePath = "game.log"

emptyScoreBoard = replicate hsSize ("...", 0)

updateHighScoreBoard :: Int -> String -> IO HighScoreBoard
updateHighScoreBoard pScore pName = do fileExist <- doesFileExist logFilePath
                                       if not fileExist
                                       then return emptyScoreBoard
                                       else do content <- readFile' logFilePath
                                               let hsBoard = update $ parse content
                                               write hsBoard
                                               return hsBoard
  where update hsBoard = take hsSize . sortBy cmpScores $ (pName, pScore) : hsBoard
        cmpScores x y = compare (snd y) (snd x)

readFile' :: FilePath -> IO String
readFile' fn = do
    h <- openFile fn ReadMode
    s <- hGetContents h
    evaluate (rnf s)
    hClose h
    return s

parse :: String -> HighScoreBoard
parse blob = maybe emptyScoreBoard parseRows $ Map.lookup "highscore" parsedFile
  where parsedFile = Map.fromList (map (tuple2 . splitOn ":") $ lines blob)
        parseRows :: String -> HighScoreBoard
        parseRows blob = map (\xs -> (head xs, (read . head $ tail xs) :: Int)) . chunksOf 2 $ splitOn ";" blob

write :: HighScoreBoard -> IO ()
write hsBoard = writeFile logFilePath line
  where line = "highscore:" ++ intercalate ";" (map (\(pname, pscore) -> pname ++ ";" ++ show pscore) hsBoard)

tuple2 :: [a] -> (a, a)
tuple2 (x : y : _) = (x, y)
