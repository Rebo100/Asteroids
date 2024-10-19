-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model
import Menu's
import Config
import Entities.Entity
import Entities.Ship
import Toolbox (drawHitboxOn)
import Animation
import Data.Maybe (fromMaybe)

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture -- Convert gamestate to something it can show on screen
viewPure gstate = Scale scale scale $ Pictures (infoPictures : buttonPictures ++ animationPictures ++ entityPictures ++ menu')
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

drawEntity :: Entity -> Picture
drawEntity entity = drawHitboxOn entity $ drawEntityType (entityType entity) -- Possible to draw hitbox ontop of entity here
  where
    drawEntityType (MkShip ship) = drawShip entity
    drawEntityType (MkAsteroid asteroid) = drawAsteroid entity
    drawEntityType (MkPowerUp powerUp) = drawPowerUp entity
    drawEntityType (MkBullet bullet) = drawBullet entity

drawShip :: Entity -> Picture
drawShip entity@Entity { entityType = MkShip ship } = color white $ translate x y rotatedShip
  where
    (x, y) = position entity
    shipAngle = angle ship
    shipSize = size entity
    -- Draw ship as a triangle
    shipShape = Polygon[ (0, shipSize), (-shipSize / 2, -shipSize / 2), (shipSize / 2, -shipSize / 2)]
    -- Draw ship when rotating
    rotatedShip = rotate (negate $ (shipAngle * 180 / pi) - 90) shipShape

drawAsteroid :: Entity -> Picture
drawAsteroid entity = color red (uncurry translate (position entity) (circleSolid (size entity)))

drawPowerUp :: Entity -> Picture
drawPowerUp = undefined

drawBullet :: Entity -> Picture
drawBullet = undefined