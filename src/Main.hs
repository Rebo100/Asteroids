 {-# language NamedFieldPuns #-}
module Main where

import Controller
import Model
import View
import Config
import Input

import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = playIO (InWindow "Asteroids" originalWindowSize (20, 20)) -- Or FullScreen
              black                 -- Background color
              60                    -- Frames per second
              initialState -- Initial state
              view                  -- View function
              input                 -- Event function
              step                  -- Step function
