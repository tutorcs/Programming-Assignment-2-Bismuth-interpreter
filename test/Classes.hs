https://tutorcs.com
WeChat: cstutorcs
QQ: 749389476
Email: tutorcs@163.com
-- | Type classes used in testing

module Classes where

import Point
import Color
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

-- | A generator for floats in range [0,1]
unitDouble = choose (0.0, 1.0)

newtype Weight = Weight Double deriving Show

-- This defines a random generator for weights
instance Arbitrary Weight where
  arbitrary = Weight <$> unitDouble

newtype Angle = Angle Double deriving Show

-- This defines a random generator for angles in [0, 2π]
instance Arbitrary Angle where
  arbitrary = Angle . (* (2 * pi)) <$> unitDouble

newtype UnitPoint = UnitPoint Point deriving Show

-- This defines a random generator for poins in [-1,1]
instance Arbitrary UnitPoint where
  arbitrary = do
    x <- choose (-1.0, 1.0)
    y <- choose (-1.0, 1.0)
    return $ UnitPoint $ Point x y

-- This defines a random generator for colors
instance Arbitrary Color where
  arbitrary = do
    r <- unitDouble
    g <- unitDouble
    b <- unitDouble
    a <- unitDouble
    return $ Color r g b a

-- random generators for points and vectors

instance Arbitrary Vector where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Vector x y

instance Arbitrary Point where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Point x y

instance Arbitrary Polar where
  arbitrary = do
    r <- arbitrary
    -- pick nice angles
    n <- choose (0, 127)
    return $ Polar (abs r * 8) (2 * pi * (fromInteger n / 128))

-- helpers for approximate equality

class ApproxEq a where
  -- | Approximate equality check, the first argument is the float threshold
  approxEq :: Double -> a -> a -> Bool

instance ApproxEq Double where
  approxEq ε x y = abs (x - y) <= max (abs x) (abs y) * ε

instance ApproxEq Point where
  approxEq ε (Point x1 y1) (Point x2 y2) = approxEqOrSmall x1 x2 && approxEqOrSmall y1 y2
    where
      approxEqOrSmall a b = approxEq ε a b || max (abs a) (abs b) < ε

instance ApproxEq Polar where
  approxEq ε (Polar r1 θ1) (Polar r2 θ2) = approxEq ε r1 r2 && approxEq ε (sin θ1) (sin θ2) && approxEq ε (cos θ1) (cos θ2)

instance ApproxEq Color where
  approxEq ε (Color r g b a) (Color r' g' b' a') = and $ map (uncurry $ approxEq ε) [(r, r'), (g, g'), (b, b'), (a, a')]

-- This operator checks that the two values are approximately the same (they are
-- within 1% of each other)
infix 1 `shouldBeAbout`
shouldBeAbout :: (HasCallStack, Show a, ApproxEq a) => a -> a -> Expectation
shouldBeAbout x y = x `shouldSatisfy` approxEq ε y
  where ε = 1e-2
