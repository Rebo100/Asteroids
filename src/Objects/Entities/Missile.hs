module Objects.Entities.Missile where
import Objects.Entities.Stats

data Missile = Missile{
    missileStats :: Stats
}

instance Eq Missile where
    (Missile stats1) == (Missile stats2) = stats1 == stats2