module Score.Score where

import qualified System.IO.Strict as S (appendFile, readFile, run)
import System.Directory (createDirectoryIfMissing)
import Objects.Entities.Entity
import Model
import Objects.Entities.Ship
import System.Directory (doesFileExist)
import Data.List (sort)
import Text.Read (readMaybe)
import Data.Maybe (mapMaybe)
import qualified Config
import qualified Control.Monad

writeScoreToFile :: Int -> IO ()
writeScoreToFile score = do
    scores <- readScores
    S.run (Control.Monad.when (null scores || (last scores < score)) $ S.appendFile Config.highScoreFilepath (show score ++ "\n")) -- Write the new score inside the file

getPlayerScore :: GameState -> Int
getPlayerScore gstate = case findPlayerShipp (entities gstate) of
    Just (Entity (MkShip ship) _ _ _ _) -> score ship
    _ -> 0  -- Default

readScores :: IO [Int]
readScores =
    S.run (S.readFile Config.highScoreFilepath >>= \content ->
        let scores = mapMaybe readMaybe (lines content) :: [Int]
            sortedScores = take 3 $ reverse $ sort scores
        in return sortedScores)