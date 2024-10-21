module Objects.Entities.Asteroid where
import Objects.Entities.Stats

-- Asteroid
data Asteroid = Asteroid
  { 
    asteroidStats :: Stats
  }
instance Eq Asteroid where
  (Asteroid stats) == (Asteroid stats2) = stats == stats2
