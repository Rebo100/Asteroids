{-# LANGUAGE NamedFieldPuns #-}
module Toolbox where

import Graphics.Gloss
import Config

drawRectangle :: [(Float, Float)] -> Picture
drawRectangle [(x, y), (x2, y2)] = Polygon [(x, y), (x, y2), (x2, y2), (x2, y)]

inRectangle :: (Float, Float) -> [(Float, Float)] -> Bool
inRectangle (a, b) [] = False
inRectangle (a, b) (_ : xs) | null xs = False
inRectangle (a, b) [(x, y), (x2, y2)] = (a >= x && a <= x2 || a <= x && a >= x2) &&
                                        (b >= y && b <= y2 || b <= y && b >= y2)

normalizeVector :: (Float, Float) -> (Float, Float)
normalizeVector (vx, vy)
  | length > 0  = (vx / length, vy / length)
  | otherwise   = (0, 0)
  where length = sqrt (vx^2 + vy^2)