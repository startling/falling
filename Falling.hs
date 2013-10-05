{-# Language TemplateHaskell #-}
{-# Language DeriveFunctor #-}
{-# Language DeriveFoldable #-}
{-# Language DeriveTraversable #-}
module Falling where
-- base
import Control.Applicative
import Data.Foldable (Foldable(..))
import Data.Function
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
infixr 5 ./
(./) :: (Functor f, Fractional b) => f b -> b -> f b
b ./ a = fmap (/ a) b

-- | Euclidean distance between the endpoints of two vectors.
distance a b = sumOf traverse . (^^ 2)
--
---- | Particles include a location, a place, and a velocity.
--data Particle n = Particle
--  { _place    :: Vector n
--  , _velocity :: Vector n
--  , _mass     :: n
--  } deriving
--  ( Eq
--  , Ord
--  , Show
--  , Functor
--  , Foldable
--  , Traversable
--  )
--makeLenses ''Particle
--
---- | Create a particle, given its place.
--particle :: Num n => Vector n -> Particle n
--particle p = Particle p 0 1
--
---- | Find the force due to gravity of one particle on another.
--gravitation :: Floating n => Particle n -> Particle n -> Vector n
--gravitation a b = recip ((distance `on` view place) a b ^ (2 :: Integer))
--  *. a ^. mass *. b ^. mass
--  *. signum (((-) `on` view place) a b)
--
---- | Move every particle by its current velocity, given a time step.
--move :: Num n => n -> Particle n -> Particle n
--move s p = place +~ (s *. view velocity p) $ p
--
---- | Let every particle in a list act on every other particle.
--update :: Floating n => [Particle n] -> [Particle n]
--update = zipping $ \b h a ->
--  velocity +~ ((grav h a + grav h b) ./ (h ^. mass)) $ h
--  where
--    -- Map over a list in a context-sensitive way.
--    zipping :: ([a] -> a -> [a] -> b) -> [a] -> [b]
--    zipping _ [] = []
--    zipping f (c : cs) = f [] c cs : zipping (\b h a -> f (c : b) h a) cs
--    -- Add the force resulting from each particle due to gravitation.
--    grav :: Floating b => Particle b -> [Particle b] -> Vector b
--    grav a = sum . map (flip gravitation a)
--
