-- | Animation module
module Animation where

import Graphics.Gloss
import Data.Maybe (fromMaybe)

data AnimationType = Flame | Explosion deriving (Show, Eq)

data Animation = Animation
  { animationType :: AnimationType,
    animationPosition :: (Float, Float), -- Position
    currentFrame :: Int, -- Current Frame
    frameTime :: Float, -- Time for every single frame
    animElapsedTime :: Float, -- Time since last frame
    totalFrames :: Int -- Total frames
  } deriving (Show, Eq)

drawAnimation :: Animation -> Picture
drawAnimation anim = animationDrawer (animationType anim) anim
  where
    animationDrawer Flame     = drawFlame
    animationDrawer Explosion = drawExplosion

-- The different frames for the flame
flameFrames :: [Picture]
flameFrames = [ 
                color red $ Polygon [(-2.5,0), (2.5,0), (0,-7.5)], 
                color orange $ Polygon [(-2,0), (2,0), (0,-7)], 
                color red $ Polygon [(-1.5,0), (1.5,0), (0,-6.5)]
              ]

-- The frames for the explosion when enemy is shot
explosionFrames :: [Picture]
explosionFrames = 
  [
    Pictures [Color red $ circleSolid 15, Color white $ Scale 0.1 0.1 $ Text "+100"],
    Pictures [Color red $ circleSolid 10, Color white $ Scale 0.1 0.1 $ Text "+100"],
    Pictures [Color red $ circleSolid 5, Color white $ Scale 0.1 0.1 $ Text "+100"]
  ]

-- draw the animation frame by frame
drawFrame :: [Picture] -> Animation -> Picture
drawFrame frames anim =
  let
    (x, y) = animationPosition anim
    frame = currentFrame anim
    pic = fromMaybe blank (lookup frame (zip [0..] frames))
  in translate x y pic

-- Draw flame frames
drawFlame :: Animation -> Picture
drawFlame = drawFrame flameFrames

-- Draw explosion frames
drawExplosion :: Animation -> Picture
drawExplosion = drawFrame explosionFrames