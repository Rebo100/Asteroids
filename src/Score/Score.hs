module Score.Score where

import System.IO (appendFile)
import System.Directory (createDirectoryIfMissing)
import Objects.Entities.Entity
import Model
import Objects.Entities.Ship

writeScoreToFile :: Int -> IO ()
writeScoreToFile score = do
    createDirectoryIfMissing True "Score" -- Create directory if it isnt there
    appendFile "Score/Highscores.txt" (show score ++ "\n") -- Write the new score inside the file

getPlayerScore :: GameState -> Int
getPlayerScore gstate = case findPlayerShipp (entities gstate) of
    Just (Entity (MkShip ship) _ _ _ _) -> score ship
    _ -> 0  -- Default

-- todo
-- Read the file and return top 3 scores?'
-- Maybe button in menu to show highscores