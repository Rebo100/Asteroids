{-# LANGUAGE NamedFieldPuns #-}
module Toolbox where

import Graphics.Gloss
import Config

-- Window resize
resize :: (Float, Float) -> Picture -> Picture
resize (x, y) p =
  let
      scaleX = (x / fromIntegral (fst originalWindowSize))
      scaleY = (y / fromIntegral (snd originalWindowSize))
   in Scale scaleX scaleY p -- Apply a scale to picture