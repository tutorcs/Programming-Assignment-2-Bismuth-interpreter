https://tutorcs.com
WeChat: cstutorcs
QQ: 749389476
Email: tutorcs@163.com
{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE DerivingVia, OverloadedRecordDot #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | Abstract syntax.

module Syntax where

import Control.Monad
import Data.List (intersperse)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Color
import Point

-- | Variables
type Var = String

-- | An expression that yields a functional image when evaluated.
data Expr =
    -- | A literal color value.  In the concrete syntax, this is a named color.
    ColorLit Color
  -- | A literal grayscale value.
  | GrayLit Double
  -- | A literal mask value.
  | MaskLit Bool
  -- | An expression that yields the x coordinate.  This corresponds to the
  -- image @getX@.
  | X
  -- | An expression that yields the y coordinate.  This corresponds to the
  -- image @getY@.
  | Y
  -- | An expression that yields the norm of a point (@r@ component of a polar value).
  | R
  -- | An expression that yields the norm of a point (@θ@ component of a polar value).
  | Theta
  -- | A variable whose value is already set.
  | VarE Var
  -- | A binary operation
  | BinOp Expr BinOp Expr
  -- | A unary operation
  | UnOp UnOp Expr
  -- | A local variable declaration. This looks like @x = e1; e2@ in the
  -- concrete syntax.
  | Let Var Expr Expr
  -- | A color expression.  In the concrete syntax, this is written as @[eR, eG, eB]@ or @[eR, eG, eB, eA]@.
  | ColorE Expr Expr Expr Expr
  -- Image transformations
  -- | Flip given image on the vertical axis.  In the concrete syntax, this
  -- looks like @flip(e)@.
  | Flip Expr
  -- | Scale given image.  In the concrete syntax, this looks like @scale(a,
  -- e)@.
  | Scale Arith Expr
  -- | Swirl given image.  In the concrete syntax, this looks like @swirl(a,
  -- e)@.
  | Swirl Arith Expr
  -- | Scale given image with different factors in different axes.  In the
  -- concrete syntax, this looks like @scale(a, a, e)@.
  | Scale2 Arith Arith Expr
  -- | Translate given image.  In the concrete syntax, this looks like
  -- @translate(a, a, e)@.
  | Translate Arith Arith Expr
  -- | Rotate given image.  In the concrete syntax, this looks like @rotate(a,
  -- e)@.
  | Rotate Arith Expr
  -- | Overlay two images.  In the concrete syntax, this looks like @overlay(e1,
  -- e2)@.
  | Overlay Expr Expr
  -- | An if expression.  In the concrete syntax, this looks like @if e1 { e2 }
  -- else { e3 }@.
  | If Expr Expr Expr
  -- | Juxtapose given list of images. In the concrete syntax, this looks like
  -- @juxtapose(e1, e2, ...)@ where e1 is the first argument and e2 is the rest
  -- of the images (this guarantees that the list of images passed to this
  -- function is not empty.
  | Juxtapose Expr [Expr]
  -- | Juxtapose two images side-by-side. In the concrete syntax, this looks
  -- like @e1 <..> e2@.
  | JuxtaposeH Expr Expr
  -- | Juxtapose two images side-by-side. In the concrete syntax, this looks like @e1 <:> e2@.
  | JuxtaposeV Expr Expr
  -- | Replicate given image given number of times.  In the concrete syntax,
  -- this looks like @replicate(a, e)@ and it is equivalent to @juxtapose(e, e,
  -- ..., e)@ where @e@ is repeated @n@ times and @n@ is the result of
  -- evaluating @a@, rounded using @round@.
  | Replicate Arith Expr
  deriving (Eq, Show)

data BinOp = Add
           | Mul
           | Div
           | Sub
           | Less
           | Eq
           | And
           | Or
           | Xor
           deriving (Eq, Show)

data UnOp = Negate
          | BuiltIn BuiltInFn
          | Not
           deriving (Eq, Show)

-- | Arithmetic expressions.  These are used for calculating parameters to
-- spatial transformations, etc.
data Arith = Pi -- ^ π
           -- | A binary operation
           | BinOpA Arith BinOpA Arith
           -- | A unary operation
           | UnOpA UnOpA Arith
           -- | A numeric constant
           | NumLit Double
           deriving (Eq, Show)

data BinOpA = AddA | MulA | DivA | SubA deriving (Eq, Show)
data UnOpA = NegateA | BuiltInA BuiltInFn deriving (Eq, Show)

-- | Built-in functions
data BuiltInFn = Sin | Cos | Tan | Sqrt
           deriving (Eq, Show)

getFn Sin  = sin
getFn Cos  = cos
getFn Tan  = tan
getFn Sqrt = sqrt

-- | A bismuth program is just an expression describing the image and the resolution.
data Program = Program { image :: Expr
                       , resolution :: (Int, Int)
                       }
             deriving (Eq, Show)
