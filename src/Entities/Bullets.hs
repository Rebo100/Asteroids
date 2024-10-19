module Entities.Bullets where

-- Bullets
data Bullet = Bullet
  { 
    count :: Int
  }
instance Eq Bullet where
  (Bullet c) == (Bullet c2) = c == c2