https://tutorcs.com
WeChat: cstutorcs
QQ: 749389476
Email: tutorcs@163.com
{-# LANGUAGE LambdaCase, FlexibleContexts #-}

module ImageSpec where

import Data.Either (fromRight)
import Data.Bifunctor (second)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck hiding (scale)
import Classes
import Color
import Image
import Example
import Rendering
import Point
import qualified Codec.Picture as P

-- Operator to generate image equality properties
imageEq im1 im2 = do
  \(UnitPoint p) -> im1 p `shouldBeAbout` im2 p

infix 1 `imageEq`

imageEqExact im1 im2 = do
  \(UnitPoint p) -> im1 p `shouldBe` im2 p

infix 1 `imageEqExact`

liftSpec = describe "lift functions" $ do
  prop "lift0 should work correctly" $ do
      lift0 red `imageEq` \p -> red
  prop "lift1 should work correctly" $ do
      lift1 injRed getX `imageEq` (\(Point x _) -> Color x 0 0 1)
  
interpolateSpec = describe "interpolateImage" $ do
  prop "should work correctly" $ do
    interpolateImage 0.7 (injRed . getX) (injBlue . getY) `imageEq` (\(Point x y) -> Color (0.7 * x) 0 (0.3 * y) 1)

overlaySpec = describe "overlayImage" $ do
  prop "should work correctly" $ do
    overlayImage (setOpacity 0.5 . injRed . getX) (injBlue . getY) `imageEq` (\(Point x y) -> Color (0.5 * x) 0 (0.5 * y) 1)

selectSpec = describe "select" $ do
  prop "should work correctly" $ do
    select inCircle getX getY `imageEq` (\p@(Point x y) -> if norm p < 1 then x else y)
    select vStrip (const red) (const blue) `imageEq` (\p -> if getX p > 0 then red else blue)

maskToColorSpec = describe "maskToColor" $ do
  prop "should convert True to black, False to white" $ do
    maskToColor vStrip `imageEq` (\p -> if getX p > 0 then black else white)
    maskToColor hStrip `imageEq` (\p -> if getY p > 0 then black else white)

maskToGraySpec = describe "maskToGray" $ do
  prop "should convert True to black, False to white" $ do
    maskToGray vStrip `imageEq` (\p -> if getX p > 0 then 0 else 1)
    maskToGray hStrip `imageEq` (\p -> if getY p > 0 then 0 else 1)

grayToColorSpec = describe "grayToColor" $ do
  prop "should convert the gray level to color" $ do
    grayToColor gradientX `imageEq` (gray . abs . getX)
    grayToColor gradientY `imageEq` (gray . abs . getY)

invertSpec = describe "invert" $ do
  prop "should invert a grayscale image" $ do
    invert gradientX `imageEq` ((1 -) . abs . getX)
    invert gradientY `imageEq` ((1 -) . abs . getY)

toPolarImSpec = prop "toPolarIm should work correctly" $ do
  \p@(Polar r _) -> toPolarIm inCircle p `shouldBe` r < 1

fromPolarImSpec = prop "fromPolarIm should work correctly" $ do
  fromPolarIm (\(Polar r _) -> r < 0.5) `imageEqExact` (\p -> norm p < 0.5)

rotateImSpec = prop "rotateIm should rotate each point accordingly" $ do
  rotateIm (pi/2) vStrip `imageEqExact` hStrip

swirlImSpec = prop "swirlIm should swirl each point accordingly" $ do
  swirlIm pi getX `imageEq` fromPolarIm (\(Polar r θ) -> getX (toPoint $ Polar r $ θ - r * pi))

scaleImSpec = prop "scaleIm should scale the image properly" $ do
  scaleIm 0.5 inCircle `imageEqExact` fromPolarIm (\(Polar r θ) -> r < 0.5)

translateImSpec = prop "translateIm should move the image in the right direction" $ do
  translateIm (Vector (-1) (-1)) vStrip `imageEqExact` \(Point x _) -> x > -1

juxtaposeHSpec = prop "(<..>) should put two images side-by-side" $ do
  const False <..> const True `imageEqExact` \(Point x _) -> x >= 0
  -- todo: extra

juxtaposeVSpec = prop "(<:>) should put the first image on top of the second image" $ do
  const False <:> const True `imageEqExact` (<= 0) . getY
  -- todo: extra

juxtaposeSpec = prop "juxtapose should put multiple images side-by-side" $ do
  juxtapose (map const [red, green, blue]) `imageEq` \case (Point x _) | x < -1/3  -> red
                                                                       | x > 1/3  -> blue
                                                                       | otherwise -> green
  juxtapose [getX, getY, const 1, const 0.5] `imageEq` \case (Point x y) | x < -0.5  -> 4 * (x + 0.75)
                                                                         | x < 0     -> y
                                                                         | x < 0.5   -> 1
                                                                         | otherwise -> 0.5

fullMSpec = prop "fullM should always yield true" $ do
  fullM `imageEqExact` const True

emptyMSpec = prop "emptyM should always yield false" $ do
  emptyM `imageEqExact` const False

notMSpec = prop "notM should negate its input" $ do
  notM vStrip `imageEqExact` \p -> getX p < 0

intersectMSpec = do
  prop "intersectM should be commutative" $ do
    intersectM hStrip vStrip `imageEqExact` intersectM vStrip hStrip
  prop "intersectM should work like liftPred (&&)" $ do
    intersectM hStrip vStrip `imageEqExact` \(Point x y) -> x > 0 && y > 0

unionMSpec = do
  prop "unionM should be commutative" $ do
    unionM hStrip vStrip `imageEqExact` unionM vStrip hStrip
  prop "unionM should work like liftPred (||)" $ do
    unionM hStrip vStrip `imageEqExact` \(Point x y) -> x > 0 || y > 0

xorMSpec = do
  prop "xorM should be commutative" $ do
    xorM hStrip vStrip `imageEqExact` xorM vStrip hStrip
  prop "xorM should work like xor of masks" $ do
    xorM hStrip vStrip `imageEqExact` not . checker 1

filterImSpec = do
  prop "filterIm should white out the parts of the mask that are not set" $ do
    filterIm hStrip (const red) `imageEq` \(Point x _) -> if x > 0 then red else white
    filterIm inCircle (maskToColor vStrip) `imageEq` \p@(Point x _) -> if norm p < 1 && x > 0 then black else white

imageSpec = describe "Image" $ do
  liftSpec
  describe "image compositions" $ do
    interpolateSpec
    overlaySpec
    selectSpec
    maskToColorSpec
    maskToGraySpec
    grayToColorSpec
    invertSpec
    filterImSpec
  describe "lifted polar-point conversion" $ do
    toPolarImSpec
    fromPolarImSpec
  describe "lifted spatial transformations" $ do
    rotateImSpec
    swirlImSpec
    scaleImSpec
    translateImSpec
  describe "juxtaposition" $ do
    juxtaposeHSpec
    juxtaposeVSpec
    juxtaposeSpec
  describe "mask algebra" $ do
    fullMSpec
    emptyMSpec
    notMSpec
    intersectMSpec
    unionMSpec
    xorMSpec

-- run the examples
examples = describe "functional image examples" $ do
  mapM_ checkExample images

checkExample :: (String, P.Image P.PixelRGBA8) -> SpecWith (Arg Expectation)
checkExample (name, image) = it (name ++ " should be rendered correctly") $ do
  imageFromFile <- P.readPng ("example-images/" ++ name ++ ".png")
  (P.ImageRGBA8 image == get imageFromFile) `shouldBe` True
  where
    get (Left _) = undefined
    get (Right r) = r

renderWithDims width height = second $ \im -> render im width height

renderExample :: (PixelValue c) => (a, Image c) -> (a, P.Image P.PixelRGBA8)
renderExample = renderWithDims 512 512

images :: [(String, P.Image P.PixelRGBA8)]
images =
  -- black-and-white images
  map renderExample
  [ ("rays", rays' 10)
  , ("chess", checker 8)
  , ("rings", rings 10)
  , ("hexagon", polygon 6)
  ] ++
  --grayscale images
  map renderExample
  [ ("gradientX", gradientX)
  , ("shadyRays", rays 5)
  , ("donut", donut 4)
  ]
  ++
  -- full-color images
  map renderExample
  [ ("fourAtOnce", fourAtOnce)
  , ("redHexagon", lift1 injRed $ maskToGray $ polygon 6)
  , ("combinedGrads", combinedGrads)
  , ("rgb", juxtapose $ map const [red, green, blue])
  ]
