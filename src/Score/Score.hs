module Score.Score where

import System.IO (appendFile)
import System.Directory (createDirectoryIfMissing)
import Objects.Entities.Entity
import Model
import Objects.Entities.Ship
import System.IO (readFile)
import System.Directory (doesFileExist)
import Data.List (sort)
import Text.Read (readMaybe)
import Data.Maybe (mapMaybe)

writeScoreToFile :: Int -> IO ()
writeScoreToFile score = 
    createDirectoryIfMissing True "Score" >> -- Create directory if it isnt there
    appendFile "Score/Highscores.txt" (show score ++ "\n") -- Write the new score inside the file

getPlayerScore :: GameState -> Int
getPlayerScore gstate = case findPlayerShipp (entities gstate) of
    Just (Entity (MkShip ship) _ _ _ _) -> score ship
    _ -> 0  -- Default

readScores :: IO [Int]
readScores = do
    exists <- doesFileExist "Score/Highscores.txt" -- Check if file exists
    if exists then do
        content <- readFile "Score/Highscores.txt"
        let scores = mapMaybe readMaybe (lines content) :: [Int]
            sortedScores = take 3 $ reverse $ sort scores
        return sortedScores -- Get the top 3 scores
    else
        return [] -- return emmpty list

updateHighScores :: GameState -> [Int] -> GameState
updateHighScores gstate scores = gstate { highScores = scores } -- Update gamestate with specific scores (in main.hs we add the scores to gstate)