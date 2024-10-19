-- | Animation module
module Animation where

import Graphics.Gloss
import Data.Maybe (fromMaybe)

-- wat voor andere animaties willen we nog meer??
-- Voor nu eerst deze 2 maken en dan verder kijken denk ik
data AnimationType = Flame | Explosion deriving (Show, Eq)

-- Animation data type
data Animation = Animation
  { animationType :: AnimationType
  , animationPosition :: (Float, Float) -- Position
  , currentFrame :: Int -- Current Frame
  , frameTime :: Float -- Time for every single frame
  , animElapsedTime :: Float -- Time since laatste frame
  , totalFrames :: Int -- Total
  } deriving (Show, Eq)

drawAnimation :: Animation -> Picture
drawAnimation anim = animationDrawer (animationType anim) anim
  where
    animationDrawer Flame = drawFlame
    animationDrawer Explosion = drawExplosion -- Voor later alvast

-- The different frames for the flame
flameFrames :: [Picture]
flameFrames = [ 
                color red $ Polygon [(-2.5,0), (2.5,0), (0,-7.5)], 
                color orange $ Polygon [(-2,0), (2,0), (0,-7)], 
                color red $ Polygon [(-1.5,0), (1.5,0), (0,-6.5)]
              ]

-- Draw frame by frame
drawFlame :: Animation -> Picture
drawFlame anim =
  let
    (x, y) = animationPosition anim -- Positie
    frame = currentFrame anim -- Huidige frame
    flamePic = fromMaybe blank (lookup frame (zip [0..] flameFrames)) -- De juiste frame van de animation
  in translate x y flamePic -- Draw flame op juiste plek

-- Placeholder for explosion
drawExplosion :: Animation -> Picture
drawExplosion anim = undefined