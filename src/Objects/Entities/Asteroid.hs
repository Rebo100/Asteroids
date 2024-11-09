module Objects.Entities.Asteroid where
import Objects.Entities.Stats

-- Asteroid
data Asteroid = Asteroid
  { 
    asteroidStats :: Stats,
    asteroidAngle :: Float
  }
instance Eq Asteroid where
  (Asteroid stats _) == (Asteroid stats2 _) = stats == stats2
