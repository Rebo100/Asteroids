{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}
module LevelLoader where

import Objects.Entities.Entity
import Model
import Data.List (intercalate, isPrefixOf, isSuffixOf)
import Config
import System.Directory
import System.FilePath
import GHC.Exts (VecCount(Vec2))
import Toolbox (getRandomScreenCoord)
import Graphics.Gloss.Data.Vector (normalizeV)
import Data.Bifunctor (bimap)
import Score.Score (readScores, writeScoreToFile)


type Level = [Entity]
type Levels = [Level]

data LevelLoader = LevelLoader
    {
        queue :: Levels
    }
levelLoader :: IO LevelLoader
levelLoader = do
    levels <- parseAllLevels
    return LevelLoader { queue = levels}

-- Public methods
loadNextLvl :: GameState -> GameState
loadNextLvl gstate =
    let
        lvl = getLvl gstate
    in gstate {levelIndex = levelIndex gstate + 1, entities = entities gstate ++ lvl}

restartLvls :: GameState -> GameState
restartLvls gstate =
    let restartedLvl = getLvl gstate {levelIndex = 0}
    in gstate { entities = playerShip : restartedLvl, levelIndex = 1}

-- Private methods
getLvl :: GameState -> Level -- Returns a lvl from the queue, unless queue is empty. Will Generate new lvl
getLvl gstate | length (levels gstate) > levelIndex gstate = levels gstate !! levelIndex gstate
              | otherwise = generateLvl

generateLvl :: Level -- todo generates lvl when no lvls are provided
generateLvl = []

-- Custom levels
-- Public
initialize :: GameState -> IO GameState
initialize gstate = do
    levels <- parseAllLevels
    writeScoreToFile 0
    scores <- readScores                                      
    return gstate { isLoaded = True, levels = levels, highScores = scores}

-- Private
createCustomLevelBlueprint :: String
createCustomLevelBlueprint = let
    scale = 100
    row = replicate scale '-'
    offset = round $ fromIntegral scale / 3
    screenH = replicate offset '-' ++ replicate (scale - 2 * offset) 'X' ++ replicate offset '-'
    screenV = replicate offset '-' ++ "X" ++ replicate (scale - 2 * offset - 2) '-' ++ "X" ++ replicate offset '-'
    screen = "\n" ++ intercalate "\n" [screenH, intercalate "\n" $ replicate 10 screenV, screenH] ++ "\n"
    in intercalate "\n" (replicate 8 row) ++ screen ++ intercalate "\n" (replicate 8 row)

createCustomLevelFile :: String -> IO ()
createCustomLevelFile = writeFile Config.customLevelFilepath

scanCustomLevels :: IO [FilePath]
scanCustomLevels = do
    let filter' = filter (\x -> isPrefixOf "Lvl" x && isSuffixOf ".txt" x)
    filePaths <- getDirectoryContents Config.customLevelFolderFilepath
    return $ reverse $ map (Config.customLevelFolderFilepath </>) (filter' filePaths)

parseAllLevels :: IO Levels
parseAllLevels = do
    filePaths <- scanCustomLevels
    mapM parseLevel filePaths

parseLevel :: FilePath -> IO Level
parseLevel path = do
    file <- readFile path
    let lines' = lines file
    parseLines lines'


parseLines :: [String] -> IO Level
parseLines xs = do
    x <- mapM (`parseLine` scaleY) zip'
    return $ concat x
    where
        zip' = zip [1 ..] xs
        scaleY = fromIntegral (snd Config.originalWindowSize) / fromIntegral (length zip')


parseLine :: (Int, String) -> Float -> IO Level
parseLine (y, s) scaleY = do
    entities <- mapM (`charToEntity` (realToFrac $ originY - scaleY * fromIntegral y)) x's
    return $ concat entities
    where
        fieldSize = fst Config.originalWindowSize
        scaleX = fromIntegral fieldSize / fromIntegral (length s)
        originX = fromIntegral fieldSize / (-2)
        originY = fromIntegral fieldSize / 2
        x's = map (\(x, s') -> (realToFrac $ originX + x * scaleX, s')) $ zip [1 ..] s


charToEntity :: (Float, Char) -> Float -> IO [Entity]
charToEntity (x, c) y | c == 'a' || c == 'A' = do
    p <- getRandomScreenCoord
    let v = bimap (speed *) (speed *) $ normalizeV (bimap (\x2 -> x2 / areaDivider - x) (\y2 -> y2 / areaDivider - y) p)
        speed = 40
        areaDivider = 2
    do
        asteroid <- createAsteroid position v 20
        return [asteroid]
                      | c == 'm' || c == 'M' = return [createMissile position (0, 0)]
                      | otherwise = return []
    where position = bimap (Config.gameScale *) (Config.gameScale *) (x, y)