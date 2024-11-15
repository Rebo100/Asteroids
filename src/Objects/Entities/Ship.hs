module Objects.Entities.Ship where
import Objects.Entities.Stats
import Objects.Entities.PowerUp
import Objects.Entities.Bullets

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
  (Ship player _ _ _ _ _ _) == (Ship player2 _ _ _ _ _ _) = player == player2
data Player = P1 | P2
instance Eq Player where
  P1 == P1 = True
  P2 == P2 = True
  _ == _ = False 

type Score = Int

type FiringRate = Float

-- Ship Methods
updateShipLives :: Ship -> Int -> Ship
updateShipLives ship@(Ship _ _ _ (Stats _ live) _ _ _) i = ship {playerStats = (playerStats ship) {lives = live + i}}