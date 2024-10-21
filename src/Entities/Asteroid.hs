module Entities.Asteroid where
import Entities.Stats

-- Asteroid
data Asteroid = Asteroid
  { 
    asteroidStats :: Stats
  }
instance Eq Asteroid where
  (Asteroid stats) == (Asteroid stats2) = stats == stats2
