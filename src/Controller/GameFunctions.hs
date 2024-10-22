module Controller.GameFunctions where
import Model
import Objects.Menu's
import Objects.Entities.Entity
import Objects.Entities.Ship
import Objects.Entities.Stats
import Animation
import Data.Maybe (mapMaybe)
import Data.List (find)
import LevelLoader (loadNextLvl, reloadLvl)
-- Pause game
pauseGame :: GameState -> GameState
pauseGame gstate | isPaused gstate = gstate {isPaused = False, menu = None}
                 | otherwise = gstate {isPaused = True, menu = pauseMenu}
-- Button functionality
doButtonFunction :: ButtonFunction -> GameState -> GameState
doButtonFunction StartGame gstate = loadNextLvl gstate { menu = None}
doButtonFunction ResumeGame gstate = pauseGame gstate
doButtonFunction ExitGame gstate = gstate { isRunning = False }
doButtonFunction RestartLvl gstate = pauseGame $ reloadLvl gstate

-- Update gamestate
updateGamestate :: Float -> GameState -> GameState
updateGamestate secs gstate = gstate
    {
        elapsedTime = elapsedTime gstate + secs,
        entities = updatedEntities,
        animations = updatedAnimations
    }
    where
        updatedEntities = map (updateEntityPosition secs (keyPressed gstate) . updatePlayerCollision (entities gstate)) (entities gstate) -- Update positions entities
        gstateWithFlame = checkFlame updatedEntities gstate -- Check if we need to create animation for flame behind shiup
        updatedAnimations = updateAnimations secs (animations gstateWithFlame) -- If so then update animation

-- Check player collision
isGameOver :: [Ship] -> Bool
isGameOver = all isPlayerDead

isPlayerDead :: Ship -> Bool
isPlayerDead (Ship _ _ _ (Stats _ lives) _ _ _) = lives <= 0

updatePlayerCollision :: [Entity] -> Entity -> Entity
updatePlayerCollision xs e@(Entity (MkShip s) _ _ _) | checkCollisions e xs = e {entityType = MkShip (updateLives s (-1))}
                                                     | otherwise = e
updatePlayerCollision _ e = e

-- Spawn flame animation
flameAnimation :: Entity -> GameState -> GameState
flameAnimation shipEntity gstate =
  case entityType shipEntity of
    MkShip ship ->
      let
        -- Get angle and pos of ship, vanuit daar kijken we waar we de flame willen plaatsen
        (shipX, shipY) = position shipEntity
        shipAngle = angle ship
        flameOffset = (-cos shipAngle * (size shipEntity),-sin shipAngle * (size shipEntity)) -- Offset (dit heeft echt te lang gekost)
        flamePos = (shipX + fst flameOffset, shipY + snd flameOffset) -- Spawn locatie voor de flame
        -- Create a new flame animatie
        flame = Animation
          { animationType = Flame
          , animationPosition = flamePos
          , currentFrame = 0
          , frameTime = 0.035  -- seconden per frame (hiermee kan je beetje rondspelen als je t iets anders wilt)
          , animElapsedTime = 0
          , totalFrames = 3
          }
      in gstate { animations = flame : animations gstate }
    _ -> gstate  -- Dont do anything for other entities

-- Update all animations
updateAnimations :: Float -> [Animation] -> [Animation]
updateAnimations secs = mapMaybe (updateAnimation secs)
    where
        -- update 1 frame, check time passed and if we have yet to reach max frames
        updateAnimation :: Float -> Animation -> Maybe Animation
        updateAnimation secs animation
            | newElapsed >= frameTime animation && nextFrame < totalFrames animation = Just animation { currentFrame = nextFrame, animElapsedTime = remainingTime }
            | newElapsed >= frameTime animation && nextFrame >= totalFrames animation = Nothing
            | otherwise = Just animation { animElapsedTime = newElapsed }
            where
                newElapsed = animElapsedTime animation + secs -- Time passed since the last frame
                nextFrame = currentFrame animation + 1 -- next frame (index + 1)
                remainingTime = newElapsed - frameTime animation -- How much time does the animation have left

-- Check if entity is a ship
findPlayerShip :: [Entity] -> Maybe Entity
findPlayerShip = find (isShip . entityType)
    where
        isShip (MkShip _) = True
        isShip _          = False

-- check if flame should be created (w pressed) otherwise just return gstate
checkFlame :: [Entity] -> GameState -> GameState
checkFlame updatedEntities gstate
    | 'w' `elem` keyPressed gstate, Just shipEntity <- findPlayerShip updatedEntities = flameAnimation shipEntity gstate
    | otherwise = gstate