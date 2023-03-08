https://tutorcs.com
WeChat: cstutorcs
QQ: 749389476
Email: tutorcs@163.com
-- | Point manipulation
module Point where

import Util (todo)

-- | Points in Cartesian coordinates
data Point = Point Double -- ^ x coordinate
                   Double -- ^ y coordinate
  deriving (Eq, Ord, Show)

getX (Point x _) = x
getY (Point _ y) = y

-- | Points in polar coordinates
data Polar = Polar Double -- ^ θ, the angle of the point
                   Double -- ^ r, the length of the point
  deriving (Eq, Ord, Show)

-- | Vectors in Cartesian coordinates.  Conceptually, they are not rooted at the
-- origin and we separate them from points by having a different type.
data Vector = Vector Double -- ^ x coordinate
                     Double -- ^ y coordinate
  deriving (Eq, Ord, Show)

-- Conversion between Cartesian and polar coordinates are given by the formulas:
--
-- x = r cos θ
-- y = r sin θ
--
-- r = √(x² + y²)
-- θ = arccos (x / r) -- if r ≠ 0
--
-- For r = 0, there are multiple representations, we will use θ = 0 as
-- the canonical representation.
--
-- Note that `acos` always returns a value in [0, π]. But θ > π when y < 0. In
-- this case, you need to return 2π - arccos(x / r).

-- | Convert a point in Cartesian coordinates to polar coordinates.
toPolar :: Point -> Polar
toPolar p@(Point x y) = todo

-- | Convert a point in polar coordinates to Cartesian coordinates.
toPoint :: Polar -> Point
toPoint (Polar r θ) = todo

-- | Norm of a point (distance from the origin).
--
-- You can use this function to implement toPolar and swirl.
norm (Point x y) = sqrt $ x * x + y * y

-- | Midpoint of two points in Cartesian coordinates.
midpoint (Point x1 y1) (Point x2 y2) = Point (avg x1 x2) (avg y1 y2)
  where avg a b = (a + b) / 2

-- | Midpoint of two points in Polar coordinates.
--
-- Hint: use midpoint, toPolar, and toPoint above to compute this. You can add
-- as many parameters to the equation as you need.
midpoint' :: Polar -> Polar -> Polar
midpoint' p1 p2 = todo

-- New spatial transformations, these functions transform coordinates in space.

-- | Rotate given image by given angle.  This is already implemented for you.
rotate angle (Point x y) = Point (c * x - s * y) (s * x + c * y)
  where c = cos angle
        s = sin angle

-- | Translate given point by given vector.  To do this, simply add the x
-- component of the vector to the x component of the point, and do the same for
-- the y components.
translate :: Vector -> Point -> Point
translate (Vector xv yv) (Point xp yp) = todo

-- | Scale given point by given scalar value.  Implement this by multiplying
-- each coordinate of the point by the given value.
scale :: Double -> Point -> Point
scale k (Point xp yp) = todo

-- | Swirl a point: rotate it by a given angle times its distance from origin.
-- That is, swirling a point p by the angle θ is equivalent to rotating it by
-- |p| × θ.
swirl :: Double -> Point -> Point
swirl θ p = todo
