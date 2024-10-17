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
import CreateEnemy
import Input
import Data.List (findIndex)
import System.Exit (exitSuccess)

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate | not (isRunning gstate) = exitSuccess
                 | isPaused gstate = return $ gstate {elapsedTime = elapsedTime gstate + secs}
                 | otherwise = do -- Update the game state
                 -- Decide if we have to add new asteroid or not
                  (newEntities, newAsteroidTime) <- createAsteroid (elapsedTime gstate + secs) gstate
                  return $ gstate { elapsedTime = elapsedTime gstate + secs, timeSinceAsteroid = newAsteroidTime, entities = map (updateEntityPosition secs (keyPressed gstate)) newEntities }