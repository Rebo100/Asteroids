module Objects.Entities.Stats where

-- Stats
type Lives = Int

type Damage = Float

data Stats = Stats
  { 
    damage :: Damage,
    lives :: Lives
  }
instance Eq Stats where
  (Stats d l) == (Stats d2 l2) = d == d2 && l == l2