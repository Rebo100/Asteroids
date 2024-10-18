module Entities.Ship where
import Entities.Stats
import Entities.PowerUp
import Entities.Bullets

-- Ship
data Ship = Ship
  { player :: Player,
    score :: Score,
    powerUp :: PowerUp,
    playerStats :: Stats,
    playerBullet :: Bullet,
    playerFiringRate :: FiringRate,
    angle :: Float
  }

data Player = P1 | P2

type Score = Int

type FiringRate = Float

-- Ship Methods
isGameOver :: Ship -> Bool
isGameOver (Ship _ _ _ (Stats _ lives) _ _ _) = lives <= 0

updateLives :: Ship -> Int -> Ship
updateLives ship@(Ship _ _ _ (Stats _ live) _ _ _) i = ship {playerStats = (playerStats ship) {lives = live + i}}