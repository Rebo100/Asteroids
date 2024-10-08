module Entities where
import Graphics.Gloss (Vector)
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

-- Ship
data Ship = Ship
  { player :: Player,
    score :: Score,
    powerUp :: PowerUp,
    playerStats :: Stats,
    playerBullet :: Bullet,
    playerFiringRate :: FiringRate
  }

data Player = P1 | P2

type Score = Int

type FiringRate = Float

-- Asteroid
data Asteroid = Asteroid
  { asteroidStats :: Stats
  }

-- Levels
data Level
  = Lvl1
  | Lvl2
  | Lvl3
  | CustomLvl
      { entities :: [Entity]
      }