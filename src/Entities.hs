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
    size :: Size,
    speed :: Float
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
    playerFiringRate :: FiringRate
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
      playerFiringRate = 1
    },
    position = (0, 0),
    vector = (0, 0),
    size = 10,
    speed = 500
  }

-- Asteroid
data Asteroid = Asteroid { asteroidStats :: Stats }

makeAsteroid :: Entity
makeAsteroid = Entity
  { entityType = MkAsteroid Asteroid 
    { asteroidStats = Stats 
      { damage = 1, 
        lives = 1 
      }
    },
    position = (0, 0),
    vector = (0, -1),
    size = 20,
    speed = 30
  }

-- Update position for entities. Take secs passed, new position and entity we want to adjust position for
updateEntityPosition :: Float -> (Float, Float) -> Entity -> Entity
-- Update the players position
updateEntityPosition secs (nx, ny) entity@Entity { entityType = MkShip _ } =
  entity { position = (x + nx * speedValue * secs, y + ny * speedValue * secs) } -- we add nx/ny (new x/y value) to the original position and multiply it by speed and time
  where
    (x, y) = position entity -- Current/old position entity
    speedValue = speed entity -- Speed attribute given in Entity.hs
-- Update the asteroid's position
updateEntityPosition secs _ entity@Entity { entityType = MkAsteroid _ } =
  entity { position = (x + vx * speedValue * secs, y + vy * speedValue * secs) }
  where
    (x, y) = position entity
    (vx, vy) = vector entity
    speedValue = speed entity
updateEntityPosition _ _ entity = undefined

-- Levels
data Level
  = Lvl1
  | Lvl2
  | Lvl3
  | CustomLvl
      { levelEntities :: [Entity]
      }