module CreateEnemy where

import Model
import Entities
import Config
import Toolbox
import System.Random (randomRIO)

-- Create an asteroid
createAsteroid :: Float -> GameState -> IO ([Entity], Float)
createAsteroid time gstate
  | asteroidInterval gstate == Nothing = return (entities gstate, timeSinceAsteroid gstate)  -- we dont want asteroids (Value for interval is nothing: See model.hs)
  | timeSinceLastAsteroid >= interval = do 
    -- Create a new asteroid
      (randomX, randomY, vectorX, vectorY) <- generateAsteroidValues -- Determine the position and direction of the asteroid
      let newAsteroid = makeAsteroid { position = (randomX, randomY), vector = (vectorX, vectorY) } -- Create asteroid through makeAsteroid function with values we jsut got
          newEntities = newAsteroid : entities gstate -- Add the asteroid to the list of entities
      return (newEntities, time) -- Return the new list of entities and time
  | otherwise = return (entities gstate, timeSinceAsteroid gstate)
  where
    -- Calc time since last asteroid was made
    timeSinceLastAsteroid = time - timeSinceAsteroid gstate
    Just interval = asteroidInterval gstate

-- Generate a random position and direction for an asteroid
generateAsteroidValues :: IO (Float, Float, Float, Float)
generateAsteroidValues = do
  let windowWidth = 400
      windowHeight = 400
  -- Generate a random number (each border has a number, randomly choose a border, then on the border randomly spawn on x and y positions)
  border <- randomRIO (1, 4) :: IO Int
  -- Generate a random x and y value
  randomX <- randomRIO (-windowWidth / 2, windowWidth / 2)
  randomY <- randomRIO (-windowHeight / 2, windowHeight / 2)
  let (x, y, (vecX, vecY))
        | border == 1 = (randomX, windowHeight / 2, normalizeVector (0 - randomX, -windowHeight / 2))
        | border == 2 = (randomX, -windowHeight / 2, normalizeVector (0 - randomX, windowHeight / 2))
        | border == 3 = (-windowWidth / 2, randomY, normalizeVector (windowWidth / 2, 0 - randomY))
        | border == 4 = (windowWidth / 2, randomY, normalizeVector (-windowWidth / 2, 0 - randomY))
  return (x, y, vecX, vecY)