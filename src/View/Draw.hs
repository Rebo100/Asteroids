module View.Draw where
import Objects.Entities.Entity
import Graphics.Gloss
import Objects.Entities.Ship
import Toolbox
import Control.Applicative (Alternative(empty))

drawEntity :: Entity -> Picture
drawEntity entity = drawHitboxOn entity $ drawEntityType (entityType entity) -- Possible to draw hitbox ontop of entity here
  where
    drawEntityType (MkShip ship) = drawShip entity
    drawEntityType (MkAsteroid asteroid) = drawAsteroid entity
    drawEntityType (MkPowerUp powerUp) = drawPowerUp entity
    drawEntityType (MkBullet bullet) = drawBullet entity

drawShip :: Entity -> Picture
drawShip entity@Entity {entityType = MkShip ship} = color white $ translate x y rotatedShip
  where
    (x, y) = position entity
    shipAngle = angle ship
    shipSize = size entity
    -- Draw ship as a triangle
    shipShape = Polygon [(0, shipSize), (-shipSize / 2,-shipSize / 2), (shipSize / 2,-shipSize / 2)]
    -- Draw ship when rotating
    rotatedShip = rotate (negate $ (shipAngle * 180 / pi) - 90) shipShape

drawAsteroid :: Entity -> Picture
drawAsteroid entity | position entity `inRectangle` [(-200, 200), (200, -200)] = color red (uncurry translate (position entity) (circleSolid (size entity)))
                    | otherwise = Pictures []



drawPowerUp :: Entity -> Picture
drawPowerUp = undefined

drawBullet :: Entity -> Picture
drawBullet = undefined