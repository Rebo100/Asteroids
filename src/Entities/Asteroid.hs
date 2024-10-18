module Entities.Asteroid where
import Entities.Stats

-- Asteroid
data Asteroid = Asteroid
  { 
    asteroidStats :: Stats,
    speed :: Float
  }
