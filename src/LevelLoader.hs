{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
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
        preMades = [lvl1, lvl2, lvl3]
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

generateLvl :: Level
generateLvl = undefined

-- Custom levels
createCustomLevelBlueprint :: String
createCustomLevelBlueprint = intercalate "\n" $ replicate 200 $ replicate 200 '-'

createCustomLevelFile :: String -> IO ()
createCustomLevelFile blueprint = writeFile Config.customLevelFilepath blueprint

scanCustomLevels :: IO [FilePath]
scanCustomLevels = do
    let filter' = filter (\x -> isPrefixOf "Lvl" x && isSuffixOf ".txt" x)
    filePaths <- getDirectoryContents Config.customLevelFilepath
    return $ filter' filePaths



























-- Premade levels
lvl1 :: Level
lvl1 = []
lvl2 :: Level
lvl2 = [asteroid]
lvl3 :: Level
lvl3 = [asteroid]