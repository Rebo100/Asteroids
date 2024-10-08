module Main where

import Controller
import Model
import View

import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = playIO (InWindow "Counter" (400, 400) (0, 0)) -- Or FullScreen
              black            -- Background color
              60              -- Frames per second
              initialState     -- Initial state
              view             -- View function
              input            -- Event function
              step             -- Step function


--      Data types
--Entities
-- data Entity location = Ship | Asteroid | PowerUp | Bullet
data Entity = Entity {
    entityType :: EntityType,
    position :: Position,
    direction :: Direction,
    speed :: Speed,
    size :: Size
}

data EntityType = MkShip Ship
            | MkAsteroid Asteroid
            | MkPowerUp PowerUp
            | MkBullet Bullet

type Position = (Float, Float)
type Direction = Int
type Speed = Float
type Size = Float

--Stats
type Lives = Int
type Damage = Float
data Stats = Stats {
    damage :: Damage,
    lives :: Lives
}

--PowerUps
data PowerUp = PowerUp {
} --TrippleShot | Invincibility | ExtraLife | SpeedBoost

emptyPowerUp :: PowerUp
emptyPowerUp = PowerUp {}

--Bullets
data Bullet = Bullet {
    count :: Int
}
--Ship
data Ship = Ship {
    player :: Player,
    score :: Score,
    powerUp :: PowerUp,
    playerStats :: Stats,
    playerBullet :: Bullet,
    playerFiringRate :: FiringRate 
}

data Player = P1 | P2
type Score = Int
type FiringRate = Float

--Asteroid
data Asteroid = Asteroid {
    asteroidStats :: Stats
}

--Levels
data Level = Lvl1 | Lvl2 | Lvl3 | CustomLvl {
    entities :: [Entity]
}

--Methods

printEntity :: Entity -> String
printEntity e = sPosition ++ sDirection ++ sSpeed ++ sSize
  where
    sPosition = show $ position e
    sDirection = show $ direction e
    sSpeed = show $ speed e
    sSize = show $ size e
