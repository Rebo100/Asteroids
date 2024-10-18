module Entities.PowerUp where

-- PowerUps
data PowerUp = PowerUp {} -- TrippleShot | Invincibility | ExtraLife | SpeedBoost

emptyPowerUp :: PowerUp
emptyPowerUp = PowerUp {}