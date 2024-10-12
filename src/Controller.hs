{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import Config
import Data.Bifunctor (Bifunctor (bimap))
import Menu's
import Toolbox
import Data.List (findIndex)

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate
  | elapsedTime gstate + secs > nO_SECS_BETWEEN_CYCLES
  = -- We show a new random number
    do randomNumber <- randomIO
       let newNumber = abs randomNumber `mod` 10
       return $ gstate { infoToShow = ShowANumber newNumber}
  | otherwise
  = -- Just update the elapsed time
    return $ gstate { elapsedTime = elapsedTime gstate + secs }

-- | Handle user input
input :: Event -> GameState -> IO GameState
input event@(EventKey {}) gstate = return (inputKey event gstate) -- Handle key / mouse presses
input (EventResize window) gstate = -- Handle window resize
  let
    (x, y) = bimap fromIntegral fromIntegral window
    scaleX = (x / fromIntegral (fst Config.originalWindowSize))
    scaleY = (y / fromIntegral (snd Config.originalWindowSize))
    scale = min scaleX scaleY
  in return $ gstate { windowScale = scale }
input (EventMotion (x, y)) gstate = return $ gstate { mousePosition = (x, y) }
input _ gstate = return gstate -- Otherwise keep the same


inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char c) _ _ _) gstate = gstate { infoToShow = ShowAChar c, keyPressed = c } -- register keyboard key
inputKey (EventKey (MouseButton LeftButton) Down _ mouse) gstate = -- register mouseClick
  let
    scaledUps = map ( map (bimap (* windowScale gstate) (* windowScale gstate)) . buttonShape) (buttons gstate)
    inRectangles = map (mouse `inRectangle`) scaledUps
  in handleClickEvent (findIndex id inRectangles) mouse gstate
inputKey _ gstate = gstate -- Non handled inputs

handleClickEvent :: Maybe Int -> (Float, Float) -> GameState -> GameState
handleClickEvent index mouse gstate = maybe gstate (\x -> buttonFunction (buttons gstate!!x) `doButtonFunction` gstate) index