-- | This module contains the data types
--   which represent the state of the game
module Model where
import Objects.Entities.Entity
import Objects.Menu's
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Config (originalWindowSize)
import Data.Bifunctor (Bifunctor(bimap))
import System.Exit (exitSuccess)
import System.Random (mkStdGen, StdGen)
import Animation

data InfoToShow = ShowNothing
                | ShowANumber Int
                | ShowAChar   Char
                | ShowHighscore Int

data GameState = GameState {
                   isRunning :: Bool,
                   isPaused :: Bool,
                   infoToShow  :: InfoToShow,
                   elapsedTime :: Float,
                   windowScale :: Float,
                   keyPressed :: [Char],
                   mousePosition :: (Float, Float),
                   entities :: [Entity],
                   buttons :: [Button],
                   menu :: Menu,
                   animations :: [Animation],
                   levelIndex :: Int
                 }

initialState :: GameState
initialState =
  let mousePos = bimap fromIntegral fromIntegral Config.originalWindowSize
  -- in GameState (ShowHighscore 0) 0 1 '-' mousePos [] startMenu
  in GameState {
    isRunning = True,
    isPaused = False,
    infoToShow = ShowHighscore 0,
    elapsedTime = 0,
    windowScale = 1,
    keyPressed = [],
    mousePosition = mousePos,
    entities = [playerShip],
    buttons = [],
    menu = startMenu,
    animations = [],
    levelIndex = 0
  }