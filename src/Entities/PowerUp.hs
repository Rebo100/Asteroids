module Entities.PowerUp where

-- PowerUps
data PowerUp = PowerUp {} -- TrippleShot | Invincibility | ExtraLife | SpeedBoost
instance Eq PowerUp where
    p == p2 = p == p2

emptyPowerUp :: PowerUp
emptyPowerUp = PowerUp {}