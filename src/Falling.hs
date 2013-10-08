{-# Language TemplateHaskell #-}
{-# Language DeriveFunctor #-}
{-# Language DeriveFoldable #-}
{-# Language DeriveTraversable #-}
module Falling where
-- base
import Prelude hiding (sum)
import Control.Applicative
import Data.Foldable (Foldable(..), sum)
-- lens
import Control.Lens

-- | 3-vectors.
data Vector a = Vector
  { _x :: a
  , _y :: a
  , _z :: a
  } deriving
  ( Eq
  , Ord
  , Show
  , Read
  , Functor
  , Foldable
  , Traversable
  )
makeLenses ''Vector

instance Applicative Vector where
  pure a = Vector a a a
  Vector f g h <*> Vector a b c = Vector (f a) (g b) (h c)

instance Num a => Num (Vector a) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  (-) = liftA2 (-)
  abs = fmap abs
  negate = fmap negate
  signum = fmap signum
  fromInteger = pure . fromInteger

instance Fractional a => Fractional (Vector a) where
  (/) = liftA2 (/)
  recip = fmap recip
  fromRational = pure . fromRational

-- | Scalar multiplication
infixr 5 *.
(*.) :: (Functor f, Num b) => b -> f b -> f b
a *. b = fmap (* a) b

-- | Scalar division
infixr 5 ./
(./) :: (Functor f, Fractional b) => f b -> b -> f b
b ./ a = fmap (/ a) b

-- | Euclidean distance between the endpoints of two vectors.
distance :: Floating a => Vector a -> Vector a -> a
distance a b = sqrt . sum $ (a - b) ^ (2 :: Int)

-- | Particles include a location, a place, and a velocity.
data Particle n = Particle
  { _place    :: Vector n
  , _velocity :: Vector n
  , _mass     :: n
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
particle :: Num n => Vector n -> Particle n
particle p = Particle p 0 1

-- | Find the force due to gravity of one particle on another.
gravitation :: Floating n => Particle n -> Particle n -> Vector n
gravitation a b = (a ^. mass * b ^. mass)
  *. recip (distance (a ^. place) (b ^. place) ^ (2 :: Int))
  *. signum (b ^. place - a ^. place)

-- | Move every particle by its current velocity, given a time step.
move :: Num n => n -> Particle n -> Particle n
move s p = place +~ (s *. view velocity p) $ p

-- | Let every particle in a list act on every other particle, changing
--   its velocity.
update ss = (`map` ss) $ \a -> (&) a . (+~) velocity
  . (./ a ^. mass) . sum . map (gravitation a) . filter (/= a) $ ss

