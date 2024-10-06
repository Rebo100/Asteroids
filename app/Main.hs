{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Main(main) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game (SpecialKey(KeyPad2))

window :: Display
window = InWindow "Nice Window" (200, 200) (10, 10)

background :: Color
background = white

drawing :: Picture
drawing = circle 80

main :: IO ()
main = display window background drawing


--Data types
--Entities
-- data Entity location = Ship | Asteroid | PowerUp | Bullet
data Entity = Ship | Asteroid | PowerUp | Bullet {
    entityAttributes :: EAttributes
}

type Position = (Int, Int)
type Direction = Int
type Speed = Float
type Size = Float

data EAttributes = EAttributes {
 pst :: Position,
 dir :: Direction,
 sp :: Speed,
 size :: Size
}

--Stats
type Lives = Int
type Damage = Float
data Stats = Stats {
    damage :: Damage,
    lives :: Lives
}

--PowerUps
data PowerUp a = TrippleShot | Invincibility | ExtraLife | SpeedBoost {
    value :: a,
    effect :: a -> a
}

emptyPowerUp :: PowerUp
emptyPowerUp = ExtraLife 0

--Bullets
data Bullet = MkBullet {
    count :: Int
}
--Ship
data Player = P1 | P2
type Score = Int
type FiringRate = Float


data PlayerInfo = PlayerInfo {
    player :: Player,
    score :: Score,
    powerUp :: PowerUp,
    playerStats :: Stats,
    playerBullet :: Bullet,
    playerFiringRate :: FiringRate
}

data Ship = MkShip {
    playerInfo :: PlayerInfo
}

--Asteroid
data Asteroid = MkAsteroid {
    asteroidStats :: Stats
}

--Levels
data Level = Lvl1 | Lvl2 | Lvl3 | CustomLvl {
    entities :: [Entity]
}


-- print :: Entity -> String
-- print eAttributes = eAttributes