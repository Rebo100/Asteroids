-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model
import Menu's
import Config
import Entities

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture -- Convert gamestate to something it can show on screen
viewPure gstate = Scale scale scale $ Pictures (infoPictures : buttonPictures ++ entityPictures)
  where
    scale = windowScale gstate
    infoPictures = case infoToShow gstate of
      ShowNothing   -> blank
      ShowANumber n -> color green (text (show n))
      ShowAChar   c -> color green (text [c])
      ShowHighscore score -> color white (text ("Highscore: " ++ show score))
    buttonPictures = map (\x -> drawButton x (mousePosition gstate) scale) (buttons gstate)
    entityPictures = map drawEntity (entities gstate)

drawEntity :: Entity -> Picture
drawEntity entity = drawEntityType (entityType entity) 
  where
    drawEntityType (MkShip ship) = drawShip entity
    drawEntityType (MkAsteroid asteroid) = drawAsteroid entity
    drawEntityType (MkPowerUp powerUp) = drawPowerUp entity
    drawEntityType (MkBullet bullet) = drawBullet entity

drawShip :: Entity -> Picture
drawShip entity = color white (uncurry translate (position entity) (circleSolid (size entity)))

drawAsteroid :: Entity -> Picture
drawAsteroid = undefined

drawPowerUp :: Entity -> Picture
drawPowerUp = undefined

drawBullet :: Entity -> Picture
drawBullet = undefined