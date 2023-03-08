https://tutorcs.com
WeChat: cstutorcs
QQ: 749389476
Email: tutorcs@163.com
{-# LANGUAGE LambdaCase #-}

module Image
     where

import Point
import Color
import Util (todo)

-- | An image is a function that maps each point to a value.
--
-- This is equivalent to defining it as:
--
-- > type Image a = Point -> a
--
-- but this definition works better with the type checker due to how GHC treats
-- type aliases.
type Image = (->) Point

-- | An image in polar coordinates. This is equivalent to
--
-- > type PolarImage a = Polar -> a
type PolarImage = (->) Polar

-- Images are functors, given a function that operate on pixel values, you can
-- lift it to operate on all values.
--
-- Haskell already defines this for functions in general, so we don't need to
-- define it.
--
-- If we were to define it, it'd be like:
-- 
-- instance Functor Image where
--   fmap = (.)

-- Aliases for common image types

-- | A mask is a 1-bit image (it maps each point to whether the point is
-- masked).  Note that this is same as a predicate used in filtering.
type ImageMask = Image Bool 

-- | A color image
type ImageColor = Image Color

-- | A grayscale image
type ImageGray = Image Grayscale

-- | Image transformers are functions that operate on an image but maintain the
-- data inside (they complement the functor structure).
--
-- A function of type `ImageT a` would only manipulate the points, so this
-- captures geometric transformations over images.
type ImageT a = Image a -> Image a

-- Image compositions

-- The next few functions lift functions operating on values to functions
-- operating on images by applying them to each point's corresponding value. You
-- can think of them as analogous to `fmap` (or the liftPred function from part
-- 1) but working on different numbers of arguments.

-- | Lift a single color to a full single-color image. It is just the constant
-- function.
lift0 :: a -> Image a
lift0 = todo

-- | Lift a function to act on all color values in an image.  This is equivalent
-- to `fmap`.
lift1 :: (a -> b) -> Image a -> Image b
lift1 = todo

-- | Lift a 2-ary function to act on all color values in an image.  This is
-- equivalent to `fmap`.
lift2 :: (a -> b -> c) -> Image a -> Image b -> Image c
lift2 = todo

-- | Lift a 3-ary function to act on all color values in an image.  This is
-- equivalent to `fmap`.
lift3 :: (a -> b -> c -> d) -> Image a -> Image b -> Image c -> Image d
lift3 = todo

-- Implement the following using th elift functions.

-- | Lift interpolate from part 1 to work on images. It takes a weight and
-- interpolates two images point-wise.
interpolateImage :: Double -> ImageColor -> ImageColor -> ImageColor
interpolateImage w = todo

-- | Lift overlay from the Color module to overlay two images rather than just
-- two colors.
overlayImage :: ImageColor -> ImageColor -> ImageColor
overlayImage = todo

-- | For each point, return a point from the first image if the mask is set to
-- true, otherwise, return the second image.
--
-- For example, if we have a mask like the following:
--
-- ffffff
-- ffttff
-- fttttf
-- fttttf
-- ffttff
-- ffffff
--
-- where f=false, t=true, and the following two images:
--
-- AAAAAA   CCCCCC
-- AAAAAA   CCCCCC
-- AAAAAA   DDDDDD
-- BBBBBB   DDDDDD
-- BBBBBB   EEEEEE
-- BBBBBB   EEEEEE
--
-- then select mask im1 im2 should yield the following image:
--
-- CCCCCC
-- CCAACC
-- DAAAAD
-- DBBBBD
-- EEBBEE
-- EEEEEE
--
-- Here, imagine that A, B, C, D, E are colors and each letter corresponds to a
-- square with side length 1/3.
--
-- You can implement this function using an anonymous function (a lambda) and
-- one of the lifting functions above.
select :: ImageMask -> Image a -> Image a -> Image a
select = todo

-- | Convert a given mask to a grayscale image where a true value in the mask
-- corresponds to black (that is, 0.0), and a false value corresponds to white
-- (1.0).
maskToGray :: ImageMask -> ImageGray
maskToGray = todo

-- | Convert a given grayscale image to a color image. Use `gray` and `invert`
-- in your implementation.
grayToColor :: ImageGray -> ImageColor
grayToColor = todo

-- | Convert a given mask to a color image where a true value in the mask
-- corresponds to black, and a false value corresponds to white. This is
-- easier using the functions above.
maskToColor :: ImageMask -> ImageColor
maskToColor = todo

-- | Invert the colors in a grayscale image.  This should be like complement but
-- it should work on grayscale images.
invert :: ImageGray -> ImageGray
invert = todo

-- | Keep only the parts of an image that are masked.  If the mask is set, then
-- the original image's value is preserved, otherwise the result should be
-- white. Implement this using one of the lift methods and one of the earlier
-- methods.
filterIm :: PixelValue a => ImageMask -> ImageT a
filterIm pred im = todo

--------------------------------------------------------------------------------

-- spatial transformations

-- Conversions between polar and Cartesian images.  Implement these using
-- `toPolar` and `toPoint`.  You may need to apply the *inverse transform* to
-- implement these (similar to other special transforms below).

fromPolarIm :: PolarImage a -> Image a
fromPolarIm im = todo
toPolarIm :: Image a -> PolarImage a
toPolarIm im = todo

-- | Transform the coordinats of an image.  The value for the point `p` in the
-- output image will be the value for the point `f p`.  In other words, this
-- function applies `f` to each input point before calculating the image value.
--
-- See flipV, and stretchX for examples of how this function is used.
transformCoord :: (Point -> Point) -> ImageT a
transformCoord f im = im . f

-- | Flip an image vertically.
flipV :: ImageT a
flipV = transformCoord $ \(Point x y) -> Point x (-y)

-- | Stretch an image on the X axis by given factor. This is done by doing the
-- inverse transform on the input points so that sampling from [-1,1]² from the
-- following image
--
-- > stretchX k im
--
-- is equivalent to sampling from [-k,k]×[-1,1] in from im, and it visually
-- stretches im by k on the horizontal axis.
stretchX k = transformCoord $ \(Point x y) -> Point (x / k) y


-- lift the spatial transformations in Point to ImageT.  Each of the functions
-- in this section are supposed to be implemented using `transformCoord` and an
-- inverse transformation.  See the documentation of each function for the
-- inverse transformations.

-- | Rotate an image by θ radians.  The inverse of this is rotating by -θ
-- radians.
rotateIm θ = todo
-- | Swirl an image by θ radians.  The inverse of this is swirling by -θ
-- radians.
swirlIm θ = todo
-- | Scale an image by the scaling factor k.  The inverse of this is scaling by
-- 1/k.
scaleIm k = todo
-- | Translate (i.e., shift) an image by given vector.  The inverse of this is
-- shifting by the opposite vector.  The opposite of a vector Vector x y is
-- Vector (-x) (-y).
translateIm (Vector x y) = todo

-- Mask algebra: Boolean algebra lifted to masks so that we can combine masks to
-- select more elaborate parts of an image.  Use the lift family of functiosn to
-- implement these.

-- | A mask that always returns True (selects all points).
fullM :: ImageMask
fullM p = todo

-- | A mask that always returns False (selects none of the points).
emptyM :: ImageMask
emptyM p = todo

-- | An operator (i.e., a function) that inverts the given mask.  In other
-- words, it produces a mask where each point's value is True if the original
-- mask's value is False, and vice versa.
notM :: ImageMask -> ImageMask
notM im = todo

-- | An operator that yields a mask that returns True iff both of the original
-- masks return True.  So, it selects only the points both of its arguments
-- select.
intersectM :: ImageMask -> ImageMask -> ImageMask
intersectM = todo

-- | An operator that yields a mask that returns True iff any of the original
-- masks return True.  So, it selects only the points at least one of its
-- arguments select.
unionM :: ImageMask -> ImageMask -> ImageMask
unionM = todo

-- | An operator that yields a mask that returns True iff the original masks
-- disagree (i.e, one mask returns True while the other returns False).  So, it
-- selects only the points that exactly one of its arguments select.
xorM :: ImageMask -> ImageMask -> ImageMask
xorM = todo

-- Combining (juxtaposing) images.

-- These declarations tell Haskell how to parse the operators (<..>) and (<:>)
-- we are going to define.
infix 5 <..>
infix 5 <:>

-- | Juxtapose two images side-by-side. The first image should be mapped to
-- [-1,1]×[-1,0), and the second image should be mapped to [-1,1]×[0,1].
--
-- The name of this operator is supposed to evoke two images that are side by
-- side (represented by the dots).
(<..>) :: Image a -> Image a -> Image a
(<..>) f g (Point x y) = todo

-- | Juxtapose two images vertically. The first image should be mapped to
-- [-1,1]×[-1,0), and the second image should be mapped to [-1,1]×[0,1]. You can
-- implement this using `<..>` and `rotateIm`.
--
-- The name of this operator is supposed to evoke an image on top of another
-- (represented by the dots).
(<:>) :: Image a -> Image a -> Image a
f <:> g = todo

-- | Juxtapose a list of images side-by-side.  This is supposed to generalize
-- (but not directly use) `<..>` to work on an arbitrary number of images.  You
-- can assume that the given list of images is non-empty.
juxtapose :: [Image a] -> Image a
juxtapose images (Point x y) = todo
