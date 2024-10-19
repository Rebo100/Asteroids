module Entities.Asteroid where
import Entities.Stats

-- Asteroid
data Asteroid = Asteroid
  { 
    asteroidStats :: Stats,
    speed :: Float
  }
instance Eq Asteroid where
  (Asteroid stats speed) == (Asteroid stats2 speed2) = stats == stats2 && speed == speed2
