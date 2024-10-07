{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE GADTs #-}
module Main(main) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game (SpecialKey(KeyPad2))

window :: Display
window = InWindow (printEntity(ShipEntity (Ship (EntityAttributes (1, 0) 0 0 0)))) (200, 200) (10, 10)

background :: Color
background = white

drawing :: Picture
drawing = circle 80

main :: IO ()
main = display window background drawing


--      Data types
--Entities
-- data Entity location = Ship | Asteroid | PowerUp | Bullet
data Entity = ShipEntity Ship
            | AsteroidEntity Asteroid
            | PowerUpEntity PowerUp
            | BulletEntity Bullet

data EntityAttributes = EntityAttributes {
 position :: Position,
 direction :: Direction,
 speed :: Speed,
 size :: Size
}

type Position = (Int, Int)
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
    powerUpAttributes :: EntityAttributes
} --TrippleShot | Invincibility | ExtraLife | SpeedBoost

emptyPowerUp :: PowerUp
emptyPowerUp = PowerUp {}

--Bullets
data Bullet = Bullet {
    bulletAttributes :: EntityAttributes,
    count :: Int
}
--Ship
data Ship = Ship {
    shipAttributes :: EntityAttributes
    -- test :: String
    -- playerInfo :: PlayerInfo
}
 

-- data ShipAttributes = ShipAttributes {
--     attributes :: EntityAttributes,
--     playerInfo :: PlayerInfo
-- }

data PlayerInfo = PlayerInfo {
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
    asteroidAttributes :: EntityAttributes,
    asteroidStats :: Stats
}

--Levels
data Level = Lvl1 | Lvl2 | Lvl3 | CustomLvl {
    entities :: [Entity]
}

--Methods
getAttributes :: Entity -> EntityAttributes
getAttributes (ShipEntity ship) = shipAttributes ship
getAttributes (AsteroidEntity asteroid) = asteroidAttributes asteroid
getAttributes (PowerUpEntity powerUp)  = powerUpAttributes powerUp
getAttributes (BulletEntity bullet)   = bulletAttributes bullet

printEntity :: Entity -> String
printEntity e = sPosition ++ sDirection ++ sSpeed ++ sSize
  where
    sPosition = show $ position a
    sDirection = show $ direction a
    sSpeed = show $ speed a
    sSize = show $ size a
    a = getAttributes e
