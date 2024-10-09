 {-# language NamedFieldPuns #-}
module Main where

import Controller
import Model
import View

import Graphics.Gloss.Interface.IO.Game

windowSize' = (400, 400)
main :: IO ()
main = playIO (InWindow "Asteroids" windowSize' (20, 20)) -- Or FullScreen
              black                 -- Background color
              60                    -- Frames per second
              (initialState windowSize') -- Initial state
              view                  -- View function
              input                 -- Event function
              step                  -- Step function