{-# Language FlexibleContexts #-}
{-# Language UndecidableInstances #-}
{-# Language GeneralizedNewtypeDeriving #-}
-- base
import Control.Applicative
-- lens
import Control.Lens
-- falling
import Falling
-- QuickCheck
import Test.QuickCheck
-- hspec
import Test.Hspec

instance Arbitrary a => Arbitrary (Vector a) where
  arbitrary = Vector <$> arbitrary <*> arbitrary <*> arbitrary

instance (Num n, Arbitrary n) => Arbitrary (Particle n) where
  arbitrary = Particle <$> arbitrary
    <*> arbitrary <*> (abs <$> arbitrary)

-- | Stupid type to improve inference for QuickCheck things.
newtype Doubled f = D (f Double)

instance Show (f Double) => Show (Doubled f) where
  show (D a) = show a

instance Arbitrary (f Double) => Arbitrary (Doubled f) where
  arbitrary = D <$> arbitrary

main :: IO ()
main = hspec $ do
  describe "distance" $ do
    it "distance a a = 0.". property $
      \(D a) -> distance a a == 0
    it "distance a b = distance b a." . property $
      \(D a) b -> distance a b == distance b a
  describe "gravitation" $ do
    it "gravitation a a = Vector NaN NaN NaN." . property $
      \(D a) -> allOf traverse isNaN $ gravitation a a
    it "gravitation a b = negate (gravitation b a)." . property $
      \(D a) b -> a == b ||
        gravitation a b == negate (gravitation b a)
    it "with a /= b, gravitation a b has no NaN." $ do
      property
        $ \(D a) b -> a == b ||
          allOf traverse (not . isNaN) (gravitation a b)
  describe "update" $ do
    it "with a ^. mass /= 0, update [a] = [a]." . property $
      \(D a) -> a ^. mass == 0 || update [a] == [a]
