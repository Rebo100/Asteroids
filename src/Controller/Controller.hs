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
import Objects.Entities.Entity
import Data.Bifunctor (Bifunctor (bimap))
import Objects.Menu's
import Toolbox
import Controller.Inputs
import System.Exit (exitSuccess)
import Controller.GameFunctions
import LevelLoader (initialize, loadNextLvl)

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate | not (isRunning gstate) = exitSuccess
                 | not $ isLoaded gstate = initialize gstate
                 | isPaused gstate = return gstate
                 | isGameOver $ toShip (getEntityType (entities gstate) [] MkShip {}) [] = gameOver gstate
                 | isWaveComing gstate = return $ loadNextLvl gstate { elapsedTime = 0 }
                 | otherwise = -- Update the game state
                               return $ updateGamestate secs gstate

-- | Handle user input
input :: Event -> GameState -> IO GameState
input event@(EventKey {}) gstate = return (inputKey event gstate) -- Handle key / mouse presses
input (EventResize window) gstate =
  -- Handle window resize
  let (x, y) = bimap fromIntegral fromIntegral window
      scaleX = (x / fromIntegral (fst Config.originalWindowSize))
      scaleY = (y / fromIntegral (snd Config.originalWindowSize))
   in return $ gstate {windowScale = (scaleX, scaleY)}
input (EventMotion (x, y)) gstate = return $ gstate {mousePosition = (x, y)}
input _ gstate = return gstate