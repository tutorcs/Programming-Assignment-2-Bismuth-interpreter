https://tutorcs.com
WeChat: cstutorcs
QQ: 749389476
Email: tutorcs@163.com
-- | Image and combinator examples.

module Example where

import Point
import Color
import Image

fourAtOnce = (sunAndRays <..> lift1 (complement . injYellow) (swirlIm pi $ rays 20)) <:> (bavaria <..> lift1 maskToGreen (polygon 5))
  where
    sunAndRays = select (scaleIm 0.5 $ inCircle) (lift0 $ yellow) (lift1 maskToRed $ rays' 10)
    bavaria = filterIm (stretchX 2 $ rotateIm (pi/8) $ checker 8) $ lift0 lightBlue
    lightBlue = Color 0 (11/16) 1 1
    injYellow x = Color x x 0 1
    maskToC c b = if b then c else white
    maskToRed = maskToC red
    maskToGreen = maskToC green

disk color = filterIm inCircle $ const color

gradientX (Point x _) = abs x
gradientY (Point _ y) = abs y

inCircle p = r < 1
  where Polar r _ = toPolar p

rays n p = 0.5 + 0.5 * (sin $ n * θ)
  where Polar r θ = toPolar p

rays' n p = 0.5 * (sin $ n * θ) >= 0
  where Polar r θ = toPolar p

checker n (Point x y) = even $ floor (x * n / 2) + floor (y * n / 2)

polygon n = pred . toPolar
  where pred (Polar r θ) = r > r'
          where r' = cos (π/n) / cos (θrem - π / n)
                π = pi
                θrem = θ - 2 * π * (fromIntegral $ floor $ n * θ / (2 * π)) / n

donut n = filterIm inCircle
  (\p -> let Polar r θ = toPolar p in 0.5 - 0.5 * (sin $ n * r * pi))

rings n p =
  let Polar r θ = toPolar p in sin (n * r * pi) >= 0

combinedGrads = lift2 (interpolate 0.5) grad1 grad2
  where grad1 = lift1 injRed $ circGrad id
        grad2 = lift1 injBlue $ circGrad $ transformCoord (rotate (pi / 2))
        circGrad f = filterIm (f inCircle) gradientY

vStrip (Point x _) = x > 0
hStrip (Point _ y) = y > 0
