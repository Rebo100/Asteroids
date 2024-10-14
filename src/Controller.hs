{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# LANGUAGE NamedFieldPuns #-}
-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import Config
import Entities
import Data.Bifunctor (Bifunctor (bimap))
import Menu's
import Toolbox
import Data.List (findIndex)
import System.Exit (exitSuccess)

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate
  | isRunning gstate =
    do -- Update the game state
      return $ gstate { elapsedTime = elapsedTime gstate + secs, entities = map (updateEntityPosition secs (playerDirection (keyPressed gstate))) (entities gstate) }
  | otherwise                       = -- Just update the time    
      exitSuccess

-- | Handle user input
input :: Event -> GameState -> IO GameState
input event@(EventKey {}) gstate = return (inputKey event gstate) -- Handle key / mouse presses
input (EventResize window) gstate = -- Handle window resize
  let
    (x, y) = bimap fromIntegral fromIntegral window
    scaleX = (x / fromIntegral (fst Config.originalWindowSize))
    scaleY = (y / fromIntegral (snd Config.originalWindowSize))
    scale  = min scaleX scaleY
  in return $ gstate { windowScale = scale }
input (EventMotion (x, y)) gstate = return $ gstate { mousePosition = (x, y) }
input _ gstate = return gstate -- Otherwise keep the same

-- Handlle key presses (Helper function for input)
inputKey :: Event -> GameState -> GameState
-- Add a char to the list if presseed down
inputKey (EventKey (Char key) Down _ _) gstate
  | key `elem` "wasd" = gstate { keyPressed = key : filter (/= key) (keyPressed gstate) }
-- Remove a char from the list if released
inputKey (EventKey (Char key) Up _ _) gstate
  | key `elem` "wasd" = gstate { keyPressed = filter (/= key) (keyPressed gstate) }
-- Handle mouse clicks on buttons
inputKey (EventKey (MouseButton LeftButton) Down _ mouse) gstate =
  let
    scaledUps = map (map (bimap (* windowScale gstate) (* windowScale gstate)) . buttonShape) (buttons gstate ++ getButtons (menu gstate))
    inRectangles = map (mouse `inRectangle`) scaledUps
  in handleClickEvent (findIndex id inRectangles) mouse gstate

-- Handle EscButton
inputKey (EventKey (SpecialKey KeyEsc) Down _ _) gstate@(GameState _ _ _ _ _ _ _ _ (StartMenu _)) = gstate {isRunning = False}
inputKey (EventKey (SpecialKey KeyEsc) Down _ _) gstate@(GameState _ _ _ _ _ _ _ _ (PauseMenu _)) = gstate {menu = None}
inputKey (EventKey (SpecialKey KeyEsc) Down _ _) gstate@(GameState _ _ _ _ _ _ _ _ None) = gstate {menu = pauseMenu}

-- Any other event
inputKey _ gstate = gstate

handleClickEvent :: Maybe Int -> (Float, Float) -> GameState -> GameState
handleClickEvent index mouse gstate = 
  let
    list = buttons gstate ++ getButtons (menu gstate)
  in maybe gstate (\x -> buttonFunction (list!!x) `doButtonFunction` gstate) index

-- Calc the direction of the player, take the list of keys pressed and return tuple of floats (position on xy field)
playerDirection :: [Char] -> (Float, Float)
playerDirection keys = normalize (foldr key (0, 0) keys)
  where
    key k (x, y)
      | k == 'w'  = (x, y + 1)
      | k == 'a'  = (x - 1, y)
      | k == 's'  = (x, y - 1)
      | k == 'd'  = (x + 1, y)
      | otherwise = (x, y)
    -- Normalize the value to prevent moving faster diagonally (Or do we want that? "Feature not a bug")
    normalize (x, y)
      | length > 0  = (x / length, y / length)
      | otherwise   = (0, 0)
      where length  = sqrt (x^2 + y^2)

-- Update position for entities. Take secs passed, new position and entity we want to adjust position for
updateEntityPosition :: Float -> (Float, Float) -> Entity -> Entity
-- Update the players position
updateEntityPosition secs (nx, ny) entity@Entity { entityType = MkShip _ } =
  entity { position = (x + nx * speedValue * secs, y + ny * speedValue * secs) } -- we add nx/ny (new x/y value) to the original position and multiply it by speed and time
  where
    (x, y) = position entity -- Current/old position entity
    speedValue = speed entity -- Speed attribute given in Entity.hs
-- Update the asteroid's position
updateEntityPosition secs _ entity@Entity { entityType = MkAsteroid _ } =
  entity { position = (x + vx * speedValue * secs, y + vy * speedValue * secs) }
  where
    (x, y) = position entity
    (vx, vy) = vector entity
    speedValue = speed entity
updateEntityPosition _ _ entity = undefined