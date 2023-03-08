https://tutorcs.com
WeChat: cstutorcs
QQ: 749389476
Email: tutorcs@163.com
-- | Rendering functions and type classes.
module Rendering where

import Point
import Color
import Image
import qualified Codec.Picture as P

-- | Sample the unit square [-1,1]² to an array of "pixels"
render :: PixelValue a => Image a -> Int -> Int -> P.Image P.PixelRGBA8
render im width height = P.generateImage gen width height
  where gen x y = toPNGPixel $ flipV im $ Point (x // width) (y // height)
        -- linearly maps integers in [0..b] to [-1, 1]. The extra +1 is for
        -- uniform sampling.
        a // b = (fromIntegral $ 2 * a + 1) / fromIntegral b - 1

render' :: Image a -> Int -> Int -> [[a]]
render' im width height = mapOverCoord sampleRow height
  where
    sampleRow y = mapOverCoord (\x->im $ Point y x) width
    -- map f over [0, 1/n, 2/n, ..., 1]
    mapOverCoord f n = f . (/ fromIntegral n) . fromIntegral <$> [0..n]
