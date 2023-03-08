https://tutorcs.com
WeChat: cstutorcs
QQ: 749389476
Email: tutorcs@163.com
module PointSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck hiding (scale)
import Classes
import Point

translateSpec = describe "translate" $ do
  it "should work on some examples" $ do
    translate (Vector 1 2) (Point 3 4) `shouldBe` Point 4 6
    translate (Vector (-10) 25) (Point 30 4) `shouldBe` Point 20 29
  prop "should satisfy 'translate v1 . translate v2 = translate (v1 + v2)' where + is vector addition" $ do
    \v1 v2 p -> translate v1 (translate v2 p) `shouldBeAbout` translate (add v1 v2) p
  prop "should satisfy translate (Vector 0 0) = id" $ do
    \p -> translate (Vector 0 0) p `shouldBeAbout` p
  prop "should satisfy 'translate v . translate (-v) = id'  where - is vector negation" $ do
    \v p -> translate v (translate (negation v) p) `shouldBeAbout` p
  where
    add (Vector x1 y1) (Vector x2 y2) = Vector (x1 + x2) (y1 + y2)
    negation (Vector x y) = Vector (-x) (-y)

scaleSpec = describe "scale" $ do
  it "should work on some examples" $ do
    scale 2 (Point 3 5) `shouldBe` Point 6 10
    scale 0 (Point 3 5) `shouldBe` Point 0 0
    scale (-0.5) (Point 10 90) `shouldBe` Point (-5) (-45)
  prop "should satisfy scale k1 . scale k2 = scale (k1 * k2)" $ do
    \k1 k2 p -> scale k1 (scale k2 p) `shouldBeAbout` scale (k1 * k2) p
  prop "should satisfy scale 1 = id" $ do
    \p -> scale 1 p `shouldBe` p

swirlSpec = describe "swirl" $ do
  it "should work on some examples" $ do
    swirl (pi/2) (Point sqrt2 sqrt2) `shouldBeAbout` (Point (-sqrt2) (-sqrt2))
    swirl (pi/2) (Point 1 0) `shouldBeAbout` (Point 0 1)
    swirl pi     (Point 1 0) `shouldBeAbout` (Point (-1) 0)
  prop "should satisfy swirl k1 . swirl (-k1) = id" $ do
    \k p -> swirl k (swirl (-k) p) `shouldBeAbout` p
  where
    sqrt2 = sqrt 2

pointSpec = describe "Point" $ do
  sequence_ [translateSpec, scaleSpec, swirlSpec]
