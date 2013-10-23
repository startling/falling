{-# Language TemplateHaskell #-}
{-# Language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Falling where
-- base
import Control.Applicative
import Data.Foldable (Foldable(..))
import Data.Monoid
-- linear
import Linear
-- lens
import Control.Lens

-- | Particles include a location, a place, and a velocity.
data Particle n = Particle
  { _place    :: !(V3 n)
  , _velocity :: !(V3 n)
  , _mass     :: !n
  } deriving
  ( Eq
  , Ord
  , Show
  , Read
  , Functor
  , Foldable
  , Traversable
  )
makeLenses ''Particle

-- | Create a particle, given its place.
particle :: Num n => V3 n -> Particle n
particle p = Particle p 0 1

-- | Find the force due to gravity of one particle on another.
gravitation :: Floating n => Particle n -> Particle n -> V3 n
gravitation a b = (a ^. mass * b ^. mass)
  *^ ((recip $ qd (a ^. place) (b ^. place))
    *^ signum (b ^. place - a ^. place))
{-# SPECIALIZE gravitation :: Particle Float -> Particle Float -> V3 Float #-}

-- | Move every particle by its current velocity, given a time step.
move :: Num n => n -> Particle n -> Particle n
move s p = place +~ (s *^ view velocity p) $ p
{-# INLINE move #-}

-- | Let every particle in a list act on every other particle, changing
--   its velocity.
update :: (Eq n, Floating n) => [Particle n] -> [Particle n]
update ss = (`map` ss) $ \a -> (&) a . (+~) velocity
  . (^/ a ^. mass) . sum . map (gravitation a) . filter (/= a) $ ss
{-# SPECIALIZE update :: [Particle Float] -> [Particle Float] #-}
