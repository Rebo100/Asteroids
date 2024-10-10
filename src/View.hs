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
viewPure gstate = resize size $ Pictures (infoPictures : buttonPictures)
  where
    size = (fromIntegral (fst $ windowSize gstate), fromIntegral (snd $ windowSize gstate))
    infoPictures = case infoToShow gstate of
      ShowNothing   -> blank
      ShowANumber n -> color green (text (show n))
      ShowAChar   c -> color green (text [c])
      ShowHighscore score -> color white (text ("Highscore: " ++ show score))
    buttonPictures = map (`drawButton` mousePosition gstate) (buttons gstate)


-- Window resize
resize :: (Float, Float) -> Picture -> Picture
resize (x, y) p =
  let
      scaleX = (x / fromIntegral (fst Config.originalWindowSize))
      scaleY = (y / fromIntegral (snd Config.originalWindowSize))
   in Scale scaleX scaleY p -- Apply a scale to picture