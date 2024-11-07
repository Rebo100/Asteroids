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
import Score.Score (getPlayerScore)

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture -- Convert gamestate to something it can show on screen
viewPure gstate = uncurry Scale scale $ Pictures (buttonPictures ++ entityPictures ++ [scorePicture] ++ animationPictures ++ menu')
  where
    scale = windowScale gstate
    scorePicture = color white $ translate (-195) 180 $ Scale 0.15 0.15 $ text ("Score: " ++ show  (getPlayerScore gstate))
    buttonPictures = map (\x -> drawButton x (mousePosition gstate) scale) (buttons gstate)
    animationPictures = map drawAnimation (animations gstate)
    entityPictures = map drawEntity (entities gstate)
    menu' = drawMenu (menu gstate) (mousePosition gstate) scale
    --border = color blue $ drawRectangle [(-(fromIntegral $ fst originalWindowSize), -(fromIntegral $ fst originalWindowSize)), (fromIntegral $ fst originalWindowSize, fromIntegral $ fst originalWindowSize)]