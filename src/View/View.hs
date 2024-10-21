-- | This module defines how to turn
--   the game state into a picture
module View.View where

import Graphics.Gloss
import Model
import Objects.Menu's
import Config
import Objects.Entities.Entity
import Objects.Entities.Ship
import Toolbox (drawHitboxOn, drawRectangle)
import Animation
import Data.Maybe (fromMaybe)
import View.Draw

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture -- Convert gamestate to something it can show on screen
viewPure gstate = Scale scale scale $ Pictures (border : infoPictures : buttonPictures ++ animationPictures ++ entityPictures ++ menu')
  where
    scale = windowScale gstate
    infoPictures = case infoToShow gstate of
      ShowNothing   -> blank
      ShowANumber n -> color green (text (show n))
      ShowAChar   c -> color green (text [c])
      ShowHighscore score -> color white (text ("Highscore: " ++ show score))
    buttonPictures = map (\x -> drawButton x (mousePosition gstate) scale) (buttons gstate)
    animationPictures = map drawAnimation (animations gstate)
    entityPictures = map drawEntity (entities gstate)
    menu' = drawMenu (menu gstate) (mousePosition gstate) scale
    border = color blue $ drawRectangle [(-(fromIntegral $ fst originalWindowSize), -(fromIntegral $ fst originalWindowSize)), (fromIntegral $ fst originalWindowSize, fromIntegral $ fst originalWindowSize)]