{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# LANGUAGE NamedFieldPuns #-}
-- | This module defines how the state changes
--   in response to time and user input
module Controller.Controller where

import Model
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import Config
import Entities
import Data.Bifunctor (Bifunctor (bimap))
import Menu's
import Toolbox
import CreateEnemy
import Controller.Inputs
import System.Exit (exitSuccess)

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate | not (isRunning gstate) = exitSuccess
                 | isPaused gstate = return $ gstate {elapsedTime = elapsedTime gstate + secs}
                 | otherwise = do -- Update the game state
                 -- Decide if we have to add new asteroid or not
                  (newEntities, newAsteroidTime) <- createAsteroid (elapsedTime gstate + secs) gstate
                  return $ gstate 
                    { 
                        elapsedTime = elapsedTime gstate + secs, 
                        timeSinceAsteroid = newAsteroidTime, 
                        entities = map (updateEntityPosition secs (keyPressed gstate)) newEntities 
                    }

-- | Handle user input
input :: Event -> GameState -> IO GameState
input event@(EventKey {}) gstate = return (inputKey event gstate) -- Handle key / mouse presses
input (EventResize window) gstate =
  -- Handle window resize
  let (x, y) = bimap fromIntegral fromIntegral window
      scaleX = (x / fromIntegral (fst Config.originalWindowSize))
      scaleY = (y / fromIntegral (snd Config.originalWindowSize))
      scale = min scaleX scaleY
   in return $ gstate {windowScale = scale}
input (EventMotion (x, y)) gstate = return $ gstate {mousePosition = (x, y)}
input _ gstate = return gstate