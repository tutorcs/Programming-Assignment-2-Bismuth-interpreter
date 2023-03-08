https://tutorcs.com
WeChat: cstutorcs
QQ: 749389476
Email: tutorcs@163.com
-- | Color manipulation

module Color where

import Util (todo)
import qualified Codec.Picture as P

-- | A color represented as floating point numbers in [0,1] for each primary
-- color.
data Color = Color Double -- ^ red
                   Double -- ^ green
                   Double -- ^ blue
                   Double -- ^ alpha (opacity). If a color is opaque, alpha
                          -- is 1, if it is fully transparent, alpha is 0.
  deriving (Eq, Show)

-- Note that the type `Color` above has a single constructor with 4 arguments
-- (there is no `|` to separate constructors, so all 4 lines describe a single
-- constructor).

-- Some accessor functions
getRed   (Color red _ _ _)   = red
getGreen (Color _ green _ _) = green
getBlue  (Color _ _ blue _)  = blue
getAlpha (Color _ _ _ alpha) = alpha

-- | An opaque color where all primary colors have the given value.
gray :: Double -> Color
gray value = Color value value value 1 -- todo

-- | Mix the two colors where the first one has weight w, and the second one has
-- the weight (1 - w). This mixes alpha too. You can assume that w ∈ [0,1].
interpolate w (Color r1 g1 b1 a1) (Color r2 g2 b2 a2) =
  Color (mix r1 r2) (mix g1 g2) (mix b1 b2) (mix a1 a2)
  where
    mix x y = w * x + (1 - w) * y

-- | Complement of a color, all channel values are reversed, except alpha.
--
-- Example: complement (Color 1 0.5 0.2 0.3) = Color 0 0.5 0.8 0.3
complement (Color r g b a) = Color (1 - r) (1 - g) (1 - b) a -- todo

-- Some colors
red    = Color 1 0 0 1
green  = Color 0 1 0 1
blue   = Color 0 0 1 1
yellow = complement blue
black  = gray 0

-- | Set the opacity of a color
setOpacity α (Color r g b _) = Color r g b α

-- | A Grayscale value is just a double where black = 0, white = 1, and shades
-- of gray are in between.
type Grayscale = Double

-- conversion from greyscale values to primary colors
injRed   x = Color x 0 0 1
injGreen x = Color 0 x 0 1
injBlue  x = Color 0 0 x 1
injBlack x = white - Color x x x 1

-- | This class describes arbitrary pixel values that can be turned into an
-- image which can be output as a PNG file.
class PixelValue a where
  toPNGPixel :: a -> P.PixelRGBA8
  white :: a

-- Instance for grayscale images
instance PixelValue Double where
  toPNGPixel v = P.PixelRGBA8 quantized quantized quantized 255
    where quantized = toEnum $ round $ v * 255
  white = 1

-- Instance for color images
instance PixelValue Color where
  toPNGPixel (Color r g b a) = P.PixelRGBA8 (quantize r) (quantize g) (quantize b) (quantize a)
    where quantize x = toEnum $ round $ x * 255
  white = Color 1 1 1 1

-- Instance for masks (True = black, False = white)
instance PixelValue Bool where
  toPNGPixel b = P.PixelRGBA8 quantized quantized quantized 255
    where quantized = if b then 0 else 255
  white = False

-- An instance of Num for Color that just pushes all operations to all of the
-- channels. This is mainly to simplify implementing some functions.
instance Num Color where
  Color r1 g1 b1 α1 + Color r2 g2 b2 α2 = Color (r1 + r2) (g1 + g2) (b1 + b2) (α1 + α2)
  -- pointwise multiplication
  Color r1 g1 b1 α1 * Color r2 g2 b2 α2 = Color (r1 * r2) (g1 * g2) (b1 * b2) (α1 + α2)
  abs = cMap abs
  negate = cMap negate

-- | Apply f to each color component. This is _like_ map, but it is not generic.
cMap :: (Double -> Double) -> Color -> Color
cMap f (Color r g b a) = Color (f r) (f g) (f b) (f a)

-- | Overlay the first color over the second color. The images are mixed
-- according to the first color's alpha value, and the final color retains the
-- second color's alpha.
overlay :: Color -> Color -> Color
overlay = todo
