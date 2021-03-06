{-# Language FlexibleContexts #-}
{-# Language UndecidableInstances #-}
{-# Language GeneralizedNewtypeDeriving #-}
-- base
import Control.Applicative
-- lens
import Control.Lens
-- linear
import Linear
-- falling
import Falling
-- QuickCheck
import Test.QuickCheck
-- hspec
import Test.Hspec

instance Arbitrary a => Arbitrary (V3 a) where
  arbitrary = V3 <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary n => Arbitrary (Particle n) where
  arbitrary = Particle <$> arbitrary <*> arbitrary <*> arbitrary

-- | Stupid type to improve inference for QuickCheck things.
newtype Doubled f = D (f Double)

instance Show (f Double) => Show (Doubled f) where
  show (D a) = show a

instance Arbitrary (f Double) => Arbitrary (Doubled f) where
  arbitrary = D <$> arbitrary

main :: IO ()
main = hspec $ do
  describe "gravitation" $ do
    it "gravitation a a = V3 NaN NaN NaN." . property $
      \(D a) -> allOf traverse isNaN $ gravitation a a
    it "gravitation a b = negate (gravitation b a)." . property $
      \(D a) b -> a == b ||
        gravitation a b == negate (gravitation b a)
    it "with a /= b, gravitation a b has no NaN." . property $
      \(D a) b -> a == b ||
        allOf traverse (not . isNaN) (gravitation a b)
  describe "update" $ do
    it "with a ^. mass /= 0, update [a] = [a]." . property $
      \(D a) -> a ^. mass == 0 || update [a] == [a]
    it "never produces NaNs." . property $
      \as -> allOf (traverse . traverse) (not . isNaN)
        $ update (as :: [Particle Double])
    it "preserves length." . property $
      \as -> length (update as) == length (as :: [Particle Double])
