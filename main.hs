{-# Language TemplateHaskell #-}
module Main where
-- base
import Data.Monoid
import Control.Applicative
-- lens
import Control.Lens
-- gloss
import Graphics.Gloss hiding (Vector)
import Graphics.Gloss.Interface.Pure.Game hiding (Vector)
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
  (Running ps) = Adding (particle $ Vector x' y' 0) ps
-- If we get a mouse movement while in 'Adding', change the
-- velocity of the new particle to the new place of the mouse;
-- keep in mind the offset of particle.
withInput (EventKey (MouseButton LeftButton) _ _ _)
  (Adding p ps) = Running (p : ps)
-- If we get a mouseclick up while in 'Adding', push the particle
-- into the list of particles and switch to 'Running'.
withInput (EventMotion (x', y')) (Adding p ps) = Adding
  (velocity .~ view place p - Vector x' y' 0 $ p) ps
-- Otherwise just ignore the input.
withInput _ i = i

-- Draw an interface.
redraw :: Interface Float -> Picture
redraw i = foldMapOf (particles . traverse) single i
  <> case i of
    Running _ -> mempty
    -- If we're in an 'Adding', draw the new particle
    -- and a line indicating its velocity.
    Adding p _ -> let d = p ^. place - p ^. velocity in
      Color red (Line
        [ (p ^. place . x, p ^. place . y)
        , (d ^. x, d ^. y)
        ]) <> single p

-- Draw a single particle as a white circle.
single :: Particle Float -> Picture
single = Translate <$> (^. place . x)
  <*> (^. place . y)
  ?? Color white (Circle 0.5)

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
