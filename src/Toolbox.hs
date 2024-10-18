{-# LANGUAGE NamedFieldPuns #-}
module Toolbox where

import Graphics.Gloss
import Config
import Entities.Entity

-- Drawing
drawRectangle :: [(Float, Float)] -> Picture
drawRectangle [(x, y), (x2, y2)] = Polygon [(x, y), (x, y2), (x2, y2), (x2, y)]

drawHitbox :: Entity -> Picture
drawHitbox e@(Entity _ position _ _) = color blue $ uncurry translate position $ circle (createHitbox e)

drawHitboxOn :: Entity -> Picture -> Picture
drawHitboxOn e p = Pictures [p, drawHitbox e]

-- Checking
inRectangle :: (Float, Float) -> [(Float, Float)] -> Bool
inRectangle (a, b) [] = False
inRectangle (a, b) (_ : xs) | null xs = False
inRectangle (a, b) [(x, y), (x2, y2)] = (a >= x && a <= x2 || a <= x && a >= x2) &&
                                        (b >= y && b <= y2 || b <= y && b >= y2)

-- Printing
printEntity :: Entity -> String
printEntity e = sPosition ++ show (vector e) ++ sSize
  where
    sPosition = show $ position e
    sSize = show $ size e
    
-- Other
normalizeVector :: (Float, Float) -> (Float, Float)
normalizeVector (vx, vy)
  | length > 0  = (vx / length, vy / length)
  | otherwise   = (0, 0)
  where length = sqrt (vx^2 + vy^2)
