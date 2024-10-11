-- | This module contains the data types
--   which represent the state of the game
module Model where
import Entities
import Menu's
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Config (originalWindowSize)
import Data.Bifunctor (Bifunctor(bimap))
data InfoToShow = ShowNothing
                | ShowANumber Int
                | ShowAChar   Char
                | ShowHighscore Int

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 5

data GameState = GameState {
                   infoToShow  :: InfoToShow,
                   elapsedTime :: Float,
                   windowScale :: Float,
                   keyPressed :: Char,
                   mousePosition :: (Float, Float),
                   entities :: [Entity],
                   buttons :: [Button]
                 }

initialState :: GameState
initialState =
  let mousePos = bimap fromIntegral fromIntegral Config.originalWindowSize
  -- in GameState (ShowHighscore 0) 0 1 '-' mousePos [] startMenu
  in GameState {
    infoToShow = ShowHighscore 0,
    elapsedTime = 0,
    windowScale = 1,
    keyPressed = '-',
    mousePosition = mousePos,
    entities = [playerShip],
    buttons = startMenu
  }