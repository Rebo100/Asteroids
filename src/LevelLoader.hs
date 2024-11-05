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

-- Methods called from somewhere else
loadNextLvl :: GameState -> GameState
loadNextLvl gstate =
    let
        lvl = getLvl $ gstate
        gst = gstate { levelIndex = levelIndex gstate + 1}
    in loadLvl gst lvl

reloadLvl :: GameState -> GameState
reloadLvl gstate = let
    players = map (\x -> x { position = (0, 0) }) $ getEntityType (entities gstate) [] MkShip {}
    in loadLvl gstate { entities = [] } $ getLvl $ gstate { entities = entities gstate ++ players}

-- Private methods
loadLvl :: GameState -> Level -> GameState -- Adds the entities of a lvl to the gamestate without changing the players
loadLvl gstate lvl =
    let players = getEntityType (entities gstate) [] MkShip{}
    in gstate
    {
        entities = players ++ lvl
    }

getLvl :: GameState -> Level -- Returns a lvl from the queue, unless queue is empty. Will Generate new lvl
getLvl gstate | length (levels gstate) > levelIndex gstate = levels gstate !! levelIndex gstate
              | otherwise = generateLvl

generateLvl :: Level -- todo generates lvl when no lvls are provided
generateLvl = undefined

-- Custom levels
-- Public
initialize :: GameState -> IO GameState
initialize gstate = do
    levels <- parseAllLevels
    return gstate { isLoaded = True, levels = levels }

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
    return $ parseLines lines'

parseLines :: [String] -> Level
parseLines xs =
    let
        zip' = zip [1 ..] xs
        scaleY = fromIntegral (snd Config.originalWindowSize) / fromIntegral (length zip')
    in concatMap (`parseLine` scaleY) zip'

parseLine :: (Int, String) -> Float -> Level
parseLine (y, s) scaleY =
    let
        fieldSize = fst Config.originalWindowSize
        scaleX = fromIntegral fieldSize / fromIntegral (length s)
        originX = fromIntegral fieldSize / (-2)
        originY = fromIntegral fieldSize / 2
        x's = map (\(x, s') -> (realToFrac $ originX + x * scaleX, s')) $ zip [1 ..] s

    in concatMap (`charToEntity` (realToFrac $ originY - scaleY * fromIntegral y)) x's

charToEntity :: (Float, Char) -> Float -> [Entity]
charToEntity (x, c) y | c == 'a' || c == 'A' = [createAsteroid (x, y) 20]
                      | c == 'm' || c == 'M' = [createMissile (x, y) (0, 0)]
                      | otherwise = []