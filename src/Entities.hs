{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Entities where
import Graphics.Gloss (Vector)
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
    position = (30, 30),
    vector = (0, 1),
    size = 10,
    speed = 1
  }

-- Asteroid
data Asteroid = Asteroid { asteroidStats :: Stats }

-- Levels
data Level
  = Lvl1
  | Lvl2
  | Lvl3
  | CustomLvl
      { levelEntities :: [Entity]
      }