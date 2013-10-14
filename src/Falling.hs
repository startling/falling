{-# Language TemplateHaskell #-}
module Falling where
-- base
import Prelude hiding (sum)
import Control.Applicative
import Data.Foldable (Foldable(..), sum)
import Data.Monoid
-- lens
import Control.Lens

-- | 3-vectors.
data Vector a = Vector
  { _x :: !a
  , _y :: !a
  , _z :: !a
  } deriving
  ( Eq
  , Ord
  , Show
  , Read
  )
makeLenses ''Vector

instance Functor Vector where
  fmap f (Vector x y z) = Vector (f x) (f y) (f z)
  {-# INLINE fmap #-}

instance Foldable Vector where
  foldMap f (Vector x y z) = f x <> f y <> f z
  {-# INLINE foldMap #-}

instance Traversable Vector where
  traverse f (Vector x y z) = Vector <$> f x <*> f y <*> f z
  {-# INLINE traverse #-}

instance Applicative Vector where
  pure a = Vector a a a
  {-# INLINE pure #-}
  Vector f g h <*> Vector a b c = Vector (f a) (g b) (h c)
  {-# INLINE (<*>) #-}

instance Num a => Num (Vector a) where
  (+) = liftA2 (+)
  {-# INLINE (+) #-}
  (*) = liftA2 (*)
  {-# INLINE (*) #-}
  (-) = liftA2 (-)
  {-# INLINE (-) #-}
  abs = fmap abs
  {-# INLINE abs #-}
  negate = fmap negate
  {-# INLINE negate #-}
  signum = fmap signum
  {-# INLINE signum #-}
  fromInteger = pure . fromInteger
  {-# INLINE fromInteger #-}

instance Fractional a => Fractional (Vector a) where
  (/) = liftA2 (/)
  {-# INLINE (/) #-}
  recip = fmap recip
  {-# INLINE recip #-}
  fromRational = pure . fromRational
  {-# INLINE fromRational #-}

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
  { _place    :: !(Vector n)
  , _velocity :: !(Vector n)
  , _mass     :: !n
  } deriving
  ( Eq
  , Ord
  , Show
  , Read
  )
makeLenses ''Particle

instance Functor Particle where
  fmap f (Particle p v m) = Particle (fmap f p) (fmap f v) (f m)
  {-# INLINE fmap #-}

instance Foldable Particle where
  foldMap f (Particle p v m) = foldMap f p <> foldMap f v <> f m
  {-# INLINE foldMap #-}

instance Traversable Particle where
  traverse f (Particle p v m) = Particle
    <$> traverse f p
    <*> traverse f v
    <*> f m
  {-# INLINE traverse #-}

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
{-# INLINE move #-}

-- | Let every particle in a list act on every other particle, changing
--   its velocity.
update :: (Eq n, Floating n) => [Particle n] -> [Particle n]
update ss = (`map` ss) $ \a -> (&) a . (+~) velocity
  . (./ a ^. mass) . sum . map (gravitation a) . filter (/= a) $ ss
{-# SPECIALIZE update :: [Particle Float] -> [Particle Float] #-}
