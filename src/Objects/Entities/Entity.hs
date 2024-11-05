{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Objects.Entities.Entity where
import Graphics.Gloss (Vector)
import System.Random (mkStdGen, randomR, StdGen)
import Data.Bifunctor (Bifunctor(bimap))
import Objects.Entities.Ship
import Objects.Entities.Asteroid
import Objects.Entities.Stats
import Objects.Entities.Bullets
import Objects.Entities.PowerUp
import Objects.Entities.Missile
import Data.List (find)

-- Entities
-- data Entity location = Ship | Asteroid | Missile | PowerUp | Bullet
data Entity = Entity
  {
    entityType :: EntityType,
    position :: Position,
    vector :: Vector,
    size :: Size,
    inWindow :: Bool
  }
instance Eq Entity where
  (Entity (MkShip ship) p v s _) == (Entity (MkShip ship2) p2 v2 s2 _) = ship == ship2 && p == p2 && v == v2 && s == s2
  (Entity (MkAsteroid asteroid) p v s _) == (Entity (MkAsteroid asteroid2) p2 v2 s2 _) = asteroid == asteroid2 && p == p2 && v == v2 && s == s2
  (Entity (MkMissile missile) p v s _) == (Entity (MkMissile missile2) p2 v2 s2 _) = missile == missile2 && p == p2 && v == v2 && s == s2
  (Entity (MkPowerUp powerUp) p v s _) == (Entity (MkPowerUp powerUp2) p2 v2 s2 _) = powerUp == powerUp2 && p == p2 && v == v2 && s == s2
  (Entity (MkBullet bullet) p v s _) == (Entity (MkBullet bullet2) p2 v2 s2 _) = bullet == bullet2 && p == p2 && v == v2 && s == s2
  _ == _ = False

data EntityType
  = MkShip Ship
  | MkAsteroid Asteroid
  | MkPowerUp PowerUp
  | MkBullet Bullet
  | MkMissile Missile
instance Eq EntityType where
  (MkShip _) == (MkShip _) = True
  (MkAsteroid _) == (MkAsteroid _) = True
  (MkPowerUp _) == (MkPowerUp _) = True
  (MkBullet _) == (MkBullet _) = True
  (MkMissile _) == (MkMissile _) = True
  _ == _ = False

type Position = (Float, Float)

type Size = Float

-- Entity Methods
isColliding :: Entity -> Entity -> Bool
isColliding e@(Entity (MkBullet _) _ _ _ _) e2@(Entity (MkShip _) _ _ _ _) = False -- Prevent ship shooting itself
isColliding e@(Entity _ p _ _ _) e2@(Entity _ p2 _ _ _) =
  let
    distance = (abs $ fst p - fst p2, abs $ snd p - snd p2)
    radiusSum = createHitbox e + createHitbox e2
  in e /= e2 && (0 >= (fst distance - radiusSum) && 0 >= (snd distance - radiusSum))

checkCollisions :: Entity -> [Entity] -> Bool
checkCollisions entity = any (`isColliding` entity)

createHitbox :: Entity -> Size
createHitbox (Entity (MkShip _) _ _ size _) = size / 3
createHitbox (Entity _ _ _ size _) = size

-- get entities
getEntityType :: [Entity] -> [Entity] -> EntityType -> [Entity] -- Entity list -> acc -> Type -> typed list
getEntityType [] xss _ = xss
getEntityType (e@(Entity et1 _ _ _ _) : xs) xss et2 | et1 == et2 = getEntityType xs (e : xss) et2
                                                   | otherwise = getEntityType xs xss et2
isEntityType :: Entity -> EntityType -> Bool
isEntityType (Entity entityType _ _ _ _) entityType2 = entityType == entityType2

-- Entity manipulation
removeEntity :: Entity -> [Entity] -> [Entity] -> [Entity]
removeEntity e [] acc = acc
removeEntity e (x : xs) acc | e == x = removeEntity e xs acc -- Don't add e
                            | otherwise = removeEntity e xs $ x : acc -- Add e

replaceEntityType :: Entity -> EntityType -> Entity
replaceEntityType e@(Entity et1 _ _ _ _) et2 = e {entityType = et2}

-- Convert to entityType
toShip :: [Entity] -> [Ship] -> [Ship]
toShip [] xss = xss
toShip ((Entity (MkShip s) _ _ _ _) : xs) xss = toShip xs (s : xss)

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
      size = 10,
      inWindow = True
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
      position = (0, 0),
      vector = (0, -10),
      size = 20,
      inWindow = False
    }

createAsteroid :: (Float, Float) -> Float -> Entity
createAsteroid p size' = asteroid { 
    size = size',
    position = p
  }

createMissile :: Position -> Vector -> Entity
createMissile pos vec =
  Entity
    { entityType =
        MkMissile
          Missile
            { missileStats = Stats { damage = 1, lives = 1 } },
      position = pos,
      vector = vec,
      size = 10,
      inWindow = False
    }

createBullet :: Position -> Vector -> Entity
createBullet pos vec = Entity
  { entityType = MkBullet Bullet { count = 1 }, 
    position = pos, 
    vector = vec, 
    size = 2,
    inWindow = True
  }

-- Update position for entities. Takes secs passed, keys pressed, and entity we want to adjust position for
updateEntityPosition :: Float -> [Char] -> Position -> Entity -> Entity
-- Update the player's position
updateEntityPosition secs keys _ entity@Entity { entityType = MkShip ship } =
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
    finalPosition = wrapPosition newPosition (size entity)            -- If so, wrap it, otherwise cords = cords ()

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
updateEntityPosition secs _ _ entity@Entity { entityType = MkAsteroid asteroid } =
  entity { position = finalPosition }
  where
    (x, y) = position entity
    (vx, vy) = vector entity
    newPosition = (x + vx * secs, y + vy * secs)
    finalPosition = wrapPosition newPosition (size entity)
-- Update the missile position
updateEntityPosition secs _ playerPos entity@Entity { entityType = MkMissile missile } =
  entity { position = newPosition, vector = (newVx, newVy) }
  where
    (missileX, missileY) = position entity -- x and y cords of the missile
    (playerX, playerY) = playerPos -- x and y of the player (ship)
    (vx, vy) = vector entity -- Vector of missile
    
    -- Calculate direction to player
    directionX = playerX - missileX
    directionY = playerY - missileY
    distance = sqrt (directionX^2 + directionY^2) -- Stelling pythagoras to calc distance 

    speed = 30  -- speed of missile
    (newVx, newVy) = (speed * directionX / distance, speed * directionY / distance) -- Calc new vector values
    -- Update position based on velocity
    newPosition = wrapPosition (missileX + newVx * secs, missileY + newVy * secs) (size entity) -- New position is old pos + vectors that are calculated above
-- update bullet position
updateEntityPosition secs _ _ entity@Entity { entityType = MkBullet _ } =
  entity { position = finalPosition }
  where
    (x, y) = position entity
    (vx, vy) = vector entity
    finalPosition = (x + vx * secs, y + vy * secs)
-- Default case for other entities
updateEntityPosition _ _ _ entity = entity

-- Get player pos
getPlayerPosition :: [Entity] -> Position
getPlayerPosition entities = maybe (0, 0) position (findPlayerShipp entities)

-- Check if entity is a ship
findPlayerShipp :: [Entity] -> Maybe Entity
findPlayerShipp = find (isShip . entityType)
    where
        isShip (MkShip _) = True
        isShip _          = False

-- Wrap entity around the screen
wrapPosition :: (Float, Float) -> Float -> (Float, Float)
wrapPosition (x, y) size = (wrapX x size, wrapY y size)
  where
    -- Screenborders
    screenLeft = -200
    screenRight = 200
    screenBottom = -200
    screenTop = 200
    -- Wrap on x coordinate
    wrapX xCoord size
      | xCoord + size < screenLeft = screenRight + size
      | xCoord - size > screenRight = screenLeft - size
      | otherwise = xCoord
    -- Wrap on y coordinate
    wrapY yCoord size
      | yCoord + size < screenBottom = screenTop + size
      | yCoord - size > screenTop = screenBottom - size
      | otherwise = yCoord