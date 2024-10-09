-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model
import Menu's

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture -- Convert gamestate to something it can show on screen
viewPure gstate = Pictures (infoPictures : buttonPictures)
  where
    infoPictures = case infoToShow gstate of
      ShowNothing   -> blank
      ShowANumber n -> color green (text (show n))
      ShowAChar   c -> color green (text [c])
      ShowHighscore score -> color white (text ("Highscore: " ++ show score))
    buttonPictures = map (`drawButton` Nothing) (buttons gstate)