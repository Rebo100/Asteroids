-- | This module contains the data types
--   which represent the state of the game
module Model where
import Entities
import Menu's
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Config (originalWindowSize)
import Data.Bifunctor (Bifunctor(bimap))
import System.Exit (exitSuccess)
import System.Random (mkStdGen, StdGen)

data InfoToShow = ShowNothing
                | ShowANumber Int
                | ShowAChar   Char
                | ShowHighscore Int

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 5

data GameState = GameState {
                   isRunning :: Bool,
                   infoToShow  :: InfoToShow,
                   elapsedTime :: Float,
                   windowScale :: Float,
                   keyPressed :: [Char],
                   mousePosition :: (Float, Float),
                   entities :: [Entity],
                   buttons :: [Button]
                 }

initialState :: GameState
initialState =
  let mousePos = bimap fromIntegral fromIntegral Config.originalWindowSize
  -- in GameState (ShowHighscore 0) 0 1 '-' mousePos [] startMenu
  in GameState {
    isRunning = True,
    infoToShow = ShowHighscore 0,
    elapsedTime = 0,
    windowScale = 1,
    keyPressed = [],
    mousePosition = mousePos,
    entities = [playerShip],
    buttons = startMenu
  }

lvl1 :: GameState
lvl1 =
  let mousePos = bimap fromIntegral fromIntegral Config.originalWindowSize
      gen = mkStdGen 42  -- Seed for the asteroids (Should be random each time, which it isnt rn)
      asteroids = makeAsteroids 20 gen  -- Generate 20 random asteroids
  in
    GameState
      { 
        isRunning = True,
        infoToShow = ShowHighscore 0,
        elapsedTime = 0,
        windowScale = 1,
        keyPressed = [],
        mousePosition = mousePos,
        entities = [playerShip] ++ asteroids,
        buttons = []
      }

  -- Button functionality
doButtonFunction :: ButtonFunction -> GameState -> GameState
doButtonFunction StartGame _ = lvl1
doButtonFunction ExitGame gstate = gstate { isRunning = False }