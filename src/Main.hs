 {-# language NamedFieldPuns #-}
module Main where

import Controller.Controller
import Model
import View.View
import Config
import Score.Score (readScores, updateHighScores)
import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = do
    scores <- readScores                                      -- Load high scores from the file
    let initialgState = updateHighScores initialState scores  -- Add scores to gstate
    playIO (InWindow "Asteroids" originalWindowSize (20, 20)) -- Or FullScreen
              black                 -- Background color
              100                   -- Frames per second
              initialgState         -- Initial state
              view                  -- View function
              input                 -- Event function
              step                  -- Step function
