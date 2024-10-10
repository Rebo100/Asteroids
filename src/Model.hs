-- | This module contains the data types
--   which represent the state of the game
module Model where
import Entities
import Menu's
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
data InfoToShow = ShowNothing
                | ShowANumber Int
                | ShowAChar   Char
                | ShowHighscore Int

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 5

data GameState = GameState {
                   infoToShow  :: InfoToShow,
                   elapsedTime :: Float,
                   windowSize :: (Int, Int),
                   keyPressed :: Char,
                   mousePosition :: (Float, Float),
                   entities :: [Entity],
                   buttons :: [Button]
                 }

initialState :: (Int, Int) -> GameState
initialState (x, y) = GameState (ShowHighscore 0) 0 (x, y) ' ' (0, 0) [] startMenu