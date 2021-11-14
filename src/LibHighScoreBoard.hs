module LibHighScoreBoard where

import Model (HighScoreBoard)

import System.Directory


hsSize :: Int
hsSize = 5

updateHighScoreBoard :: Int -> String -> IO HighScoreBoard
updateHighScoreBoard pScore pName = do fileExist <- doesFileExist "game.log"
                                       if not fileExist
                                       then return emptyScoreBoard
                                       else do content <- readFile "game.log"
                                               return $ parse content
  where emptyScoreBoard = replicate hsSize ("...", 0)

parse :: String -> HighScoreBoard
parse = undefined
