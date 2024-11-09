module View.Draw where
import Objects.Entities.Entity
import Graphics.Gloss
import Objects.Entities.Ship
import Toolbox
import Control.Applicative (Alternative(empty))
import Objects.Entities.Asteroid

drawEntity :: Entity -> Picture
drawEntity entity = drawHitboxOn entity $ drawEntityType (entityType entity) -- Possible to draw hitbox ontop of entity here
  where
    drawEntityType (MkShip ship) = drawShip entity
    drawEntityType (MkAsteroid asteroid) = drawAsteroid entity
    drawEntityType (MkMissile missile) = drawMissile entity
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
drawAsteroid (Entity { entityType = MkAsteroid asteroid, position = (x, y), size = size' }) =
  color white $ translate x y $ rotate (asteroidAngle asteroid) $ scale (size' / 8) (size' / 8) asteroidShape
  where
    asteroidShape = Line
      [ (8, 10), (11, 5), (10, -3), (6, -10), (0, -8),
        (-5, -12), (-9, -7), (-11, 1), (-7, 8), (-1, 6), (7, 10) ]

drawMissile :: Entity -> Picture
drawMissile entity@Entity { entityType = MkMissile _ } = color yellow $ translate x y rotatedMissile
  where
    (x, y) = position entity -- Position of missile
    (vx, vy) = vector entity -- Vector
    missileSize = size entity -- Size
    -- Calculate the angle met hulp van vectors
    -- https://en.wikipedia.org/wiki/Atan2#:~:text=atan2(y%2C%20x)%20returns,programming%20language%20Fortran%20in%201961.
    missileAngle = atan2 vy vx
    -- Shape missile (Driehoek)
    missileShape = Polygon
      [ 
        (0, missileSize),
        (-missileSize / 2, -missileSize), 
        (missileSize / 2, -missileSize)          
      ]
    -- Rotate the missile shape to align with its movement direction
    rotatedMissile = rotate (negate $ missileAngle * 180 / pi - 90) missileShape


drawPowerUp :: Entity -> Picture
drawPowerUp = undefined

drawBullet :: Entity -> Picture
drawBullet entity = color green (uncurry translate (position entity) (circleSolid (size entity)))