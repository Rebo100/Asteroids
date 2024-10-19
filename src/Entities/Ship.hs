module Entities.Ship where
import Entities.Stats
import Entities.PowerUp
import Entities.Bullets

-- Ship
data Ship = Ship
  { 
    player :: Player,
    score :: Score,
    powerUp :: PowerUp,
    playerStats :: Stats,
    playerBullet :: Bullet,
    playerFiringRate :: FiringRate,
    angle :: Float
  }
instance Eq Ship where
  (Ship player score powerUp playerStats playerBullet playerFiringRate angle) == 
    (Ship player2 score2 powerUp2 playerStats2 playerBullet2 playerFiringRate2 angle2) = 
      player == player2 
      && score == score2 
      && powerUp == powerUp2 
      && playerStats == playerStats2 
      && playerBullet == playerBullet2 
      && playerFiringRate == playerFiringRate2 
      && angle == angle2
data Player = P1 | P2
instance Eq Player where
  p == p2 = p == p2

type Score = Int

type FiringRate = Float

-- Ship Methods
updateLives :: Ship -> Int -> Ship
updateLives ship@(Ship _ _ _ (Stats _ live) _ _ _) i = ship {playerStats = (playerStats ship) {lives = live + i}}