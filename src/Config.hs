module Config where

type WindowSize = (Int, Int) 
originalWindowSize :: WindowSize
originalWindowSize = (400, 400)

gameScale :: Float
gameScale = 3.25 -- Determines entity closeness.

customLevelFolderFilepath :: FilePath
customLevelFolderFilepath = "src\\Levels\\" -- Path to the folder containing custom levels

customLevelFilepath :: FilePath
customLevelFilepath = "src\\Levels\\Lvl.txt" -- Path for generating new customLevel-placeholder file

waveTimer :: Float
waveTimer = 15 -- Seconds