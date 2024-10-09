{-# LANGUAGE NamedFieldPuns #-}
module Toolbox where

import Graphics.Gloss
import Config

-- Window resize
-- scalePoint :: (Float, Float) -> GameState -> (Float, Float) -- Convert a point when resizing window
-- scalePoint (x, y) (GameState {windowScale = (xScale, yScale)}) = (x*xScale, y*yScale)

-- scalePoints :: [(Float, Float)] -> GameState -> [(Float, Float)] -- Convert points when resizing window
-- scalePoints [] _ = []
-- scalePoints xs gstate = map (`scalePoint` gstate) xs


-- Function to resize the p based on window dimensions
-- resize :: (Float, Float) -> Picture -> Picture
-- resize (width, height) p =
--   let -- Calculate scale factors for width and height
--       scaleFactor = min (width / fromIntegral (fst originalWindowSize)) (height / fromIntegral (snd originalWindowSize))
--    in Scale scaleFactor scaleFactor p