module Input where

import Model
import Graphics.Gloss.Interface.IO.Game
import Config 
import Toolbox
import Data.Bifunctor (Bifunctor (bimap))
import Data.List (findIndex)
import Menu's
import System.Exit (exitSuccess)

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
-- input _ gstate = return gstate -- Otherwise keep the same (VSCode gaf een warning dat hij redundant was, dus voor nu even weggecomment)

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
inputKey (EventKey (SpecialKey KeyEsc) Down _ _) gstate@(GameState _ _ _ _ _ _ _ _ _ (StartMenu _) _ _) = gstate {isRunning = False}
inputKey (EventKey (SpecialKey KeyEsc) Down _ _) gstate = pauseGame gstate
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