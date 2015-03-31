{-# Language TemplateHaskell #-}
module Main where
-- base
import Data.Monoid
import Control.Applicative
-- linear
import Linear
-- lens
import Control.Lens
-- gloss
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
-- colour
import Data.Colour (Colour)
import Data.Colour.RGBSpace
import Data.Colour.RGBSpace.HSV
-- falling
import Falling

-- | The interface can be in two states: one, where everything
--   is just running by itself, and another, where we're adding
--   and changing things about one particular particle.
data Interface n
  = Running
    { _particles :: [Particle n]
    }
  | Adding
    { _adding    :: Particle n
    , _particles :: [Particle n]
    }
  deriving
  ( Eq
  , Ord
  , Show
  )
makeLenses ''Interface
makePrisms ''Interface

-- | Do something with input.
withInput :: Event -> Interface Float -> Interface Float
-- If we get a mouseclick down in 'Running', add a particle at the
-- place of the click and switch to 'Adding'; we'll be editing
-- it until the corresponding mouseclick up.
withInput (EventKey (MouseButton LeftButton) Down _ (x', y'))
  (Running ps) = Adding (particle $ V3 x' y' 0) ps
-- If we get a mouse movement while in 'Adding', change the
-- velocity of the new particle to the new place of the mouse;
-- keep in mind the offset of particle.
withInput (EventKey (MouseButton LeftButton) _ _ _)
  (Adding p ps) = Running (p : ps)
-- If we get a mouseclick up while in 'Adding', push the particle
-- into the list of particles and switch to 'Running'.
withInput (EventMotion (x', y')) (Adding p ps) = Adding
  (velocity .~ view place p - V3 x' y' 0 $ p) ps
-- Otherwise just ignore the input.
withInput _ i = i

-- Draw an interface.
redraw :: Interface Float -> Picture
redraw i = foldMapOf (particles . traverse) single i
  <> case i of
    Running _ -> mempty
    -- If we're in an 'Adding', draw the new particle
    -- and a line indicating its velocity.
    Adding p _ -> let
        -- The vector being drawn from the particle.
        d :: V3 Float       
        d = p ^. place - p ^. velocity
        -- The color to draw the vector in.
        c :: Color
        c = uncurryRGB makeColor
          (hsv (angle (d ^. _x) (d ^. _y)) 0.5 0.5) 1 in
      -- Draw the particle we're adding and a line from it
      -- to the mouse, representing the inverse of its
      -- velocity.
      single p <> Color c (Line
        [ (p ^. place . _x, p ^. place . _y)
        , (d ^. _x, d ^. _y)
        ])
  where
    -- Radians to degrees.
    degrees :: Floating t => t -> t
    degrees = (*) (180 / pi)
    -- Angle of a vector, in degrees.
    angle :: Floating t => t -> t -> t
    angle x y = degrees . atan $ y / x

-- Draw a single particle as a white circle.
single :: Particle Float -> Picture
single = Translate <$> (^. place . _x)
  <*> (^. place . _y)
  ?? Color white (circleSolid 2.0)

-- A massive particle at the origin.
sun :: Particle Float
sun = Particle 0 0 200

-- Iterate the world.
iteration :: Float -> Interface Float -> Interface Float
iteration t i = if has _Adding i then i
  else particles %~ update . map (move t) $ i

main :: IO ()
main = play (InWindow "falling!" (300, 300) (100, 100))
  black 40 (Running [sun]) redraw withInput iteration
