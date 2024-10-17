{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Entities where
import Graphics.Gloss (Vector)
import System.Random (mkStdGen, randomR, StdGen)


-- Entities
-- data Entity location = Ship | Asteroid | PowerUp | Bullet
data Entity = Entity
  { entityType :: EntityType,
    position :: Position,
    vector :: Vector,
    size :: Size
  }

data EntityType
  = MkShip Ship
  | MkAsteroid Asteroid
  | MkPowerUp PowerUp
  | MkBullet Bullet

type Position = (Float, Float)
type Size = Float

-- Stats
type Lives = Int
type Damage = Float

data Stats = Stats
  { damage :: Damage,
    lives :: Lives
  }

-- PowerUps
data PowerUp = PowerUp {} -- TrippleShot | Invincibility | ExtraLife | SpeedBoost

emptyPowerUp :: PowerUp
emptyPowerUp = PowerUp {}

-- Bullets
data Bullet = Bullet
  { count :: Int
  }

data Player = P1 | P2
type Score = Int
type FiringRate = Float

-- Ship
data Ship = Ship
  { player :: Player,
    score :: Score,
    powerUp :: PowerUp,
    playerStats :: Stats,
    playerBullet :: Bullet,
    playerFiringRate :: FiringRate,
    angle :: Float
  }

-- Create a player ship and set all of the initial values
playerShip :: Entity
playerShip = Entity
  { entityType = MkShip Ship
    { player = P1,
      score = 0,
      powerUp = emptyPowerUp,
      playerStats = Stats { damage = 1, lives = 3 },
      playerBullet = Bullet { count = 1 },
      playerFiringRate = 1,
      angle = pi / 2
    },
    position = (0, 0),
    vector = (0, 0),
    size = 10
  }

-- Asteroid
data Asteroid = Asteroid { asteroidStats :: Stats, speed :: Float }

makeAsteroid :: Entity
makeAsteroid = Entity
  { entityType = MkAsteroid Asteroid 
    { asteroidStats = Stats 
      { damage = 1, 
        lives = 1
      },
      speed = 30
    },
    position = (0, 0),
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
    speedValue = speed asteroid 
    newPosition = (x + vx * speedValue * secs, y + vy * speedValue * secs)
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

-- Levels
data Level
  = Lvl1
  | Lvl2
  | Lvl3
  | CustomLvl
      { levelEntities :: [Entity]
      }