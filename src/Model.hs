-- | This module contains the data types
--   which represent the state of the game
module Model where
import Entities
data InfoToShow = ShowNothing
                | ShowANumber Int
                | ShowAChar   Char
                | ShowHighscore Int

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 5

data GameState = GameState {
                   infoToShow  :: InfoToShow, 
                   elapsedTime :: Float,
                   entities :: [Entity]
                 }

initialState :: GameState
initialState = GameState (ShowHighscore 0) 0 []