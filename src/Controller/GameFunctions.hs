module Controller.GameFunctions where
import Model
import Menu's
import Entities.Entity
import Entities.Ship
import Entities.Stats
-- Pause game
pauseGame :: GameState -> GameState
pauseGame gstate@(GameState _ paused _ _ _ _ _ _ _ _ _ _) | paused = gstate {isPaused = False, menu = None}
                                                      | otherwise = gstate {isPaused = True, menu = pauseMenu}
-- Button functionality
doButtonFunction :: ButtonFunction -> GameState -> GameState
doButtonFunction StartGame _ = lvl1
doButtonFunction ResumeGame gstate = pauseGame gstate
doButtonFunction ExitGame gstate = gstate { isRunning = False }

-- Update gamestate
updateGamestate :: Float -> GameState -> GameState
updateGamestate secs gstate = gstate
    {
        elapsedTime = elapsedTime gstate + secs,
        entities = map (updateEntityPosition secs (keyPressed gstate)) $ entities gstate
    }

-- Check player collision
isGameOver :: [Ship] -> Bool
isGameOver = all isPlayerDead

isPlayerDead :: Ship -> Bool
isPlayerDead (Ship _ _ _ (Stats _ lives) _ _ _) = lives <= 0




updatePlayerCollision :: [Entity] -> [Entity]
updatePlayerCollision xs = 
    let
        ships = getEntityType xs [] MkShip {}
        updatedList = map (`updatePlayerCollision'` xs) ships
    in concat updatedList

updatePlayerCollision' :: Entity -> [Entity] -> [Entity]
updatePlayerCollision' e@(Entity (MkShip s) _ _ _) xs | checkCollisions e xs = newEntity : newList
                                                      | otherwise = xs
                                                      where 
                                                        newList = removeEntity e xs []
                                                        newShip = updateLives s (-1)
                                                        newEntity = e { entityType = MkShip newShip }

    
    
    
    {-- let
        players = getEntityType (entities gstate) [] MkShip {}
        asteroids = getEntityType (entities gstate) [] MkAsteroid {}
        updatedPlayers = map (foldr func [] asteroids) players 
          where 
            func p acc | checkCollisions p asteroids = replaceEntityType p (MkShip $ updateLives (MkShip (entityType p)) -1) : acc
                       | otherwise = p : acc
    in gstate { entities = updatedPlayers }
    --}