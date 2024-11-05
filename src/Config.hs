module Config where

type WindowSize = (Int, Int) 
originalWindowSize :: WindowSize
originalWindowSize = (400, 400)

customLevelFolderFilepath :: FilePath
customLevelFolderFilepath = "src\\Levels\\"

customLevelFilepath :: FilePath
customLevelFilepath = "src\\Levels\\Lvl.txt"

waveTimer :: Float
waveTimer = 15