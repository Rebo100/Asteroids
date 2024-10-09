-- | This module contains the data types
--   which represent the state of the game
module Model where
import Entities
import Menu's
import Graphics.Gloss
data InfoToShow = ShowNothing
                | ShowANumber Int
                | ShowAChar   Char
                | ShowHighscore Int

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 5

data GameState = GameState {
                   infoToShow  :: InfoToShow,
                   elapsedTime :: Float,
                   entities :: [Entity],
                   buttons :: [Button]
                 }

initialState :: GameState
initialState = GameState (ShowHighscore 0) 0 [] [Button "" [(0, 0), (200, 200)] (makeColorI 141 141 141 255)]