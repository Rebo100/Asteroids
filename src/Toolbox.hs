module Toolbox where

import Graphics.Gloss

drawRectangle :: (Float, Float) -> (Float, Float) -> Picture
drawRectangle (x, y) (x2, y2) = Polygon [(x, y), (x, y2), (x2, y), (x2, y2)]