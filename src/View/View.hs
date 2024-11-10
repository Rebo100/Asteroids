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
import Score.Score ( getPlayerScore, readScores )

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture -- Convert gamestate to something it can show on screen
viewPure gstate = uncurry Scale scale $ Pictures (buttonPictures ++ entityPictures ++ [scorePicture] ++ animationPictures ++ menu' ++ highscorePictures)
  where
    scale = windowScale gstate
    scorePicture = color white $ translate (-195) 180 $ Scale 0.15 0.15 $ text ("Score: " ++ show  (getPlayerScore gstate))
    buttonPictures = map (\x -> drawButton x (mousePosition gstate) scale) (buttons gstate)
    animationPictures = map drawAnimation (animations gstate)
    entityPictures = map drawEntity (entities gstate)
    menu' = drawMenu (menu gstate) (mousePosition gstate) scale
    highscorePictures = case menu gstate of
      HighscoreMenu _ ->
        let background = color (black) $ translate (-110) 130 $ rectangleSolid 400 400
            text = translate (-100) 100 $ Scale 0.3 0.3 $ color white $ Text "Highscores:"
        in [background, text] ++ drawHighscores (highScores gstate)
      _ -> []