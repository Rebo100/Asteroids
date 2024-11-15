{-# OPTIONS_GHC -Wno-missing-fields #-}
module Controller.Inputs where

import Model
import Graphics.Gloss.Interface.IO.Game
import Config 
import Toolbox
import Data.Bifunctor (Bifunctor (bimap))
import Data.List (findIndex)
import Objects.Menu's
import System.Exit (exitSuccess)
import Controller.GameFunctions

-- Handlle key presses (Helper function for input)
inputKey :: Event -> GameState -> GameState
-- Add a char to the list if pressed down
inputKey (EventKey (Char key) Down _ _) gstate
  | key `elem` "wasd" = gstate { keyPressed = key : filter (/= key) (keyPressed gstate) }
-- Remove a char from the list if released
inputKey (EventKey (Char key) Up _ _) gstate
  | key `elem` "wasd" = gstate { keyPressed = filter (/= key) (keyPressed gstate) }
-- Space to shoot bullet
inputKey (EventKey (SpecialKey KeySpace) Down _ _) gstate = shootBullet gstate
-- Handle mouse clicks on buttons and shoot bullet
inputKey (EventKey (MouseButton LeftButton) Down _ mouse) gstate =
  let
    scaledUps = map (map (bimap (* fst (windowScale gstate)) (* snd (windowScale gstate))) . buttonShape) (buttons gstate ++ getButtons (menu gstate))
    inRectangles = map (mouse `inRectangle`) scaledUps
  in handleClickEvent (findIndex id inRectangles) mouse (shootBullet gstate)

-- Handle EscButton
inputKey (EventKey (SpecialKey KeyEsc) Down _ _) gstate | menu gstate == StartMenu {} = gstate {isRunning = False}
                                                        | menu gstate == HighscoreMenu {} = gstate {menu = startMenu}
                                                        | otherwise = pauseGame gstate
-- Any other event
inputKey _ gstate = gstate

handleClickEvent :: Maybe Int -> (Float, Float) -> GameState -> GameState
handleClickEvent index mouse gstate =
  let
    list = buttons gstate ++ getButtons (menu gstate)
  in maybe gstate (\x -> buttonFunction (list!!x) `doButtonFunction` gstate) index

-- Calc the direction of the player, take the list of keys pressed and return tuple of floats (position on xy field)
playerDirection :: [Char] -> (Float, Float)
playerDirection keys = normalizeVector (foldr key (0, 0) keys)
  where
    key k (x, y)
      | k == 'w'  = (x, y + 1)
      | k == 'a'  = (x - 1, y)
      | k == 's'  = (x, y - 1)
      | k == 'd'  = (x + 1, y)
      | otherwise = (x, y)