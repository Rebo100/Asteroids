-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model
import Menu's
import Config

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture -- Convert gamestate to something it can show on screen
viewPure gstate = Scale scale scale $ Pictures (infoPictures : buttonPictures)
  where
    scale = windowScale gstate
    infoPictures = case infoToShow gstate of
      ShowNothing   -> blank
      ShowANumber n -> color green (text (show n))
      ShowAChar   c -> color green (text [c])
      ShowHighscore score -> color white (text ("Highscore: " ++ show score))
    buttonPictures = map (\x -> drawButton x (mousePosition gstate) scale) (buttons gstate)