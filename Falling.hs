{-# Language TemplateHaskell #-}
{-# Language DeriveFunctor #-}
{-# Language DeriveFoldable #-}
{-# Language DeriveTraversable #-}
module Falling where
-- base
import Control.Applicative
import Control.Monad
import Data.Foldable (Foldable(..))
import Data.Function
import Data.Monoid
-- mtl
import Control.Monad.State
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
(./) :: (Functor f, Fractional b) => f b -> b -> f b
b ./ a = fmap (/ a) b

-- | Euclidean distance between the endpoints of two vectors.
distance :: Floating a => Vector a -> Vector a -> a
distance b a = signum d * (abs d ** recip 3) where
  d = delta x a b + delta y a b + delta z a b
  delta l = ((-) `on` view l)

-- | Particles include a location, a place, and a velocity.
data Particle n = Particle
  { _place    :: Vector n
  , _velocity :: Vector n
  , _mass     :: n
  } deriving
  ( Eq
  , Ord
  , Show
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
gravitation a b = recip ((distance `on` view place) a b ^ 2)
  *. a ^. mass *. b ^. mass
  *. signum (((-) `on` view place) a b)

-- | Move every particle by its current velocity, given a time step.
move :: Num n => n -> Particle n -> Particle n
move s x = place +~ (s *. view velocity x) $ x

-- | Let every particle in a list act on every other particle.
update :: Floating n => [Particle n] -> [Particle n]
update = zipping $ \b h a ->
  velocity +~ ((grav h a + grav h b) ./ (h ^. mass)) $ h
  where
    zipping :: ([a] -> a -> [a] -> b) -> [a] -> [b]
    zipping f [] = []
    zipping f (x : xs) = f [] x xs : zipping (\b h a -> f (x:b) h a) xs
    grav :: Floating b => Particle b -> [Particle b] -> Vector b
    grav a = sum . map (flip gravitation a)

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
  , Functor
  , Foldable
  , Traversable
  )
makeLenses ''Interface
makePrisms ''Interface
