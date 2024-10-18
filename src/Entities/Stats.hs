module Entities.Stats where

-- Stats
type Lives = Int

type Damage = Float

data Stats = Stats
  { 
    damage :: Damage,
    lives :: Lives
  }