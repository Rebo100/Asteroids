{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Entities.Entity where
import Graphics.Gloss (Vector)
import System.Random (mkStdGen, randomR, StdGen)
import Data.Bifunctor (Bifunctor(bimap))
import Entities.Ship
import Entities.Asteroid
import Entities.Stats
import Entities.Bullets
import Entities.PowerUp


-- Entities
-- data Entity location = Ship | Asteroid | PowerUp | Bullet
data Entity = Entity
  {
    entityType :: EntityType,
    position :: Position,
    vector :: Vector,
    size :: Size
  }
instance Eq Entity where
  (Entity (MkShip ship) p v s) == (Entity (MkShip ship2) p2 v2 s2) = ship == ship2 && p == p2 && v == v2 && s == s2
  (Entity (MkAsteroid asteroid) p v s) == (Entity (MkAsteroid asteroid2) p2 v2 s2) = asteroid == asteroid2 && p == p2 && v == v2 && s == s2
  (Entity (MkPowerUp powerUp) p v s) == (Entity (MkPowerUp powerUp2) p2 v2 s2) = powerUp == powerUp2 && p == p2 && v == v2 && s == s2
  (Entity (MkBullet bullet) p v s) == (Entity (MkBullet bullet2) p2 v2 s2) = bullet == bullet2 && p == p2 && v == v2 && s == s2
  _ == _ = False

data EntityType
  = MkShip Ship
  | MkAsteroid Asteroid
  | MkPowerUp PowerUp
  | MkBullet Bullet
instance Eq EntityType where
  (MkShip _) == (MkShip _) = True
  (MkAsteroid _) == (MkAsteroid _) = True
  (MkPowerUp _) == (MkPowerUp _) = True
  (MkBullet _) == (MkBullet _) = True
  _ == _ = False

type Position = (Float, Float)

type Size = Float

-- Entity Methods
isColliding :: Entity -> Entity -> Bool
isColliding e@(Entity _ p _ _) e2@(Entity _ p2 _ _) =
  let
    distance = (abs $ fst p - fst p2, abs $ snd p - snd p2)
    radiusSum = createHitbox e + createHitbox e2
  in e /= e2 && (0 >= (fst distance - radiusSum) && 0 >= (snd distance - radiusSum))

checkCollisions :: Entity -> [Entity] -> Bool
checkCollisions entity = any (`isColliding` entity)

createHitbox :: Entity -> Size
createHitbox (Entity (MkShip _) _ _ size) = size / 3
createHitbox (Entity _ _ _ size) = size

-- get entities
getEntityType :: [Entity] -> [Entity] -> EntityType -> [Entity] -- Entity list -> acc -> Type -> typed list
getEntityType [] xss _ = xss
getEntityType (e@(Entity et1 _ _ _ ) : xs) xss et2 | et1 == et2 = getEntityType xs (e : xss) et2
                                                   | otherwise = getEntityType xs xss et2
isEntityType :: Entity -> EntityType -> Bool
isEntityType (Entity entityType _ _ _) entityType2 = entityType == entityType2

-- Entity manipulation
removeEntity :: Entity -> [Entity] -> [Entity] -> [Entity]
removeEntity e [] acc = acc
removeEntity e (x : xs) acc | e == x = removeEntity e xs acc -- Don't add e
                            | otherwise = removeEntity e xs $ x : acc -- Add e

replaceEntityType :: Entity -> EntityType -> Entity
replaceEntityType e@(Entity et1 _ _ _) et2 = e {entityType = et2}

-- Convert to entityType
toShip :: [Entity] -> [Ship] -> [Ship]
toShip [] xss = xss
toShip ((Entity (MkShip s) _ _ _) : xs) xss = toShip xs (s : xss)

-- Defining entities
playerShip :: Entity
playerShip =
  Entity
    { entityType =
          MkShip Ship
            { player = P1,
              score = 0,
              powerUp = emptyPowerUp,
              playerStats = Stats {damage = 1, lives = 1},
              playerBullet = Bullet {count = 1},
              playerFiringRate = 1,
              angle = pi / 2
            },
      position = (0, 0),
      vector = (0, 0),
      size = 10
    }

asteroid :: Entity
asteroid =
  Entity
    { entityType =
        MkAsteroid
          Asteroid
            { asteroidStats =
                Stats
                  { damage = 1,
                    lives = 1
                  }
            },
      position = (50, 50),
      vector = (0, -1),
      size = 20
    }

-- Update position for entities. Takes secs passed, keys pressed, and entity we want to adjust position for
updateEntityPosition :: Float -> [Char] -> Entity -> Entity
-- Update the player's position
updateEntityPosition secs keys entity@Entity { entityType = MkShip ship } =
  entity { position = finalPosition, vector = (newVx, newVy), entityType = MkShip newShip }
  where
    currentAngle  = angle ship                                          -- Angle of the ship
    rotationSpeed = 2 * pi                                              -- speed at which the ship rotates

    deltaAngle  = rotationSpeed * secs * rotationDirection            -- Calc change of angle after key is pressed (speed of rotation * time * the direction we want to rotate to)
    newAngle    = currentAngle + deltaAngle                           -- Add the delta angle calculated above to the current angle to calc new angle

    thrustValue = shipThrust * thrustBool                             -- When we press W, we want to thrust, so we do thrust * 1 to "reset the value"
    shipThrust  = 200                                                 -- Thrust acceleration per second

    (vx, vy) = vector entity                                            -- Tuple of current velocity
    (ax, ay) = (thrustValue * cos newAngle, thrustValue * sin newAngle) -- Calc hoeveel we in x and y richting moeten bewegen

    frictionPerSecond = 0.7                         -- Value to slow down ship if no keys are pressed
    friction          = frictionPerSecond ** secs  -- Maak het exponentieel

    newVx = (vx + ax * secs) * friction -- Nieuwe x cord met friciton
    newVy = (vy + ay * secs) * friction -- Nieuwe y cord met friction

    (x, y) = position entity                            -- Current position
    newPosition = (x + newVx * secs, y + newVy * secs)  -- Check if we have to wrap the new position (so current pos + the movement over x and y values)
    finalPosition = wrapPosition newPosition            -- If so, wrap it, otherwise cords = cords ()

    newShip = ship { angle = newAngle }                 -- update the angle of the ship

    -- a to move left, d to move right, neither or both to not rotate aat all
    rotationDirection
      | 'a' `elem` keys && 'd' `elem` keys = 0
      | 'a' `elem` keys                    = 1
      | 'd' `elem` keys                    = -1
      | otherwise                          = 0
    -- Thrust ot not
    thrustBool
      | 'w' `elem` keys = 1
      | otherwise       = 0
-- Update the asteroid's position
updateEntityPosition secs _ entity@Entity { entityType = MkAsteroid asteroid } =
  entity { position = finalPosition }
  where
    (x, y) = position entity
    (vx, vy) = vector entity
    newPosition = (x + vx * secs, y + vy * secs)
    finalPosition = wrapPosition newPosition
-- Default case for other entities
updateEntityPosition _ _ entity = entity


wrapPosition :: (Float, Float) -> (Float, Float)
wrapPosition (x, y) = (wrap x, wrap y)
  where
    wrap cord
      | cord < -210 = cord + 410
      | cord > 210  = cord - 410
      | otherwise    = cord