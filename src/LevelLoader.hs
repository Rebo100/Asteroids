{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}
module LevelLoader where

import Objects.Entities.Entity
import Model
import Data.List (intercalate, isPrefixOf, isSuffixOf)
import Config
import System.Directory


type Level = [Entity]
type Levels = [Level]

data LevelLoader = LevelLoader
    {
        queue :: Levels
    }
levelLoader =
    let
        preMades = [[]]
    in LevelLoader
    {
        queue = preMades
    }

-- Methods called from somewhere else
loadNextLvl :: GameState -> GameState
loadNextLvl gstate =
    let
        lvl = getLvl $ levelIndex gstate
        gst = gstate { levelIndex = levelIndex gstate + 1}
    in loadLvl gst lvl

reloadLvl :: GameState -> GameState
reloadLvl gstate = let
    players = map (\x -> x { position = (0, 0) }) $ getEntityType (entities gstate) [] MkShip {}
    in loadLvl gstate { entities = [] } $ getLvl $ levelIndex gstate { entities = entities gstate ++ players}

-- Private methods
loadLvl :: GameState -> Level -> GameState -- Adds the entities of a lvl to the gamestate without changing the players
loadLvl gstate lvl =
    let players = getEntityType (entities gstate) [] MkShip{}
    in gstate
    {
        entities = players ++ lvl
    }

getLvl :: Int -> Level -- Returns a lvl from the queue, unless queue is empty. Will Generate new lvl
getLvl index | length (queue levelLoader) > index = queue levelLoader !! index
             | otherwise = generateLvl

generateLvl :: Level -- todo generates lvl when no lvls are provided
generateLvl = undefined

-- Custom levels
-- Public
initialize :: GameState -> IO GameState
initialize gstate = do
    levels <- parseAllLevels
    let filePaths = scanCustomLevels
    return gstate { isLoaded = True, levels = levels }

-- Private
createCustomLevelBlueprint :: String
createCustomLevelBlueprint = let
    row = replicate 100 '-' 
    screenH = replicate 15 '-' ++ "X" ++ replicate 15 '-' ++ "X" ++ replicate 15 '-'
    screenV = replicate 15 '-' ++ replicate 17 'X' ++ replicate 15 '-'
    screen = intercalate "\n" [screenV, concat $ replicate 13 screenH, screenV]
    in intercalate "\n" $ replicate 100 $ replicate 100 '-'

createCustomLevelFile :: String -> IO ()
createCustomLevelFile = writeFile Config.customLevelFilepath

scanCustomLevels :: IO [FilePath]
scanCustomLevels = do
    let filter' = filter (\x -> isPrefixOf "Lvl" x && isSuffixOf ".txt" x)
    filePaths <- getDirectoryContents Config.customLevelFolderFilepath
    return $ filter' filePaths

parseAllLevels :: IO [Level]
parseAllLevels = do
    filePaths <- scanCustomLevels 
    let lvls = map parseLevel filePaths 
    return lvls

parseLevel :: FilePath -> Level -- todo
parseLevel _ = []