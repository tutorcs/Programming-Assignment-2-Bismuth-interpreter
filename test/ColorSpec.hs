https://tutorcs.com
WeChat: cstutorcs
QQ: 749389476
Email: tutorcs@163.com
module ColorSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck hiding (scale)
import Classes
import Color

colorSpec = describe "Color" $ do
  describe "overlay" $ do
    prop "should mix red, green, blue according to the first image's alpha" $ do
      \c1 c2 ->
        let c  = overlay c1 c2
            c' = interpolate (getAlpha c1) c1 c2 in
          do
            getRed c `shouldBe` getRed c'
            getGreen c `shouldBe` getGreen c'
            getBlue c `shouldBe` getBlue c'
    prop "should retain the second image's alpha" $ do
      \c1 c2 ->
        getAlpha (overlay c1 c2) `shouldBe` getAlpha c2
