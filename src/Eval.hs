https://tutorcs.com
WeChat: cstutorcs
QQ: 749389476
Email: tutorcs@163.com
{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE DerivingVia, OverloadedRecordDot, FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | The interpreter for bismuth programs.

module Eval where

import Control.Applicative (liftA2, liftA3)
import Control.Monad
import Data.List (intersperse)
import Data.Maybe (fromMaybe, fromJust)
import Data.Map.Strict (Map, (!?))
import qualified Data.Map.Strict as M
import qualified Codec.Picture as P

import Point
import Image
import Color
import Domains
import Syntax
import Rendering
import Util

-- | A representation of runtime errors.
data RuntimeError = RuntimeError String
                  | ParseError String

-- | Helper function for returning errors, when returning an error, just use
-- @err "message"@ and put a descriptive message.
err = Left . RuntimeError

-- | Helper function for converting @Maybe a@ to @Either RuntimeError a@.  You
-- can implement and use this helper to simplify how you use some of the liftV
-- functions.  This function should return a @Left@ value with given error
-- message if its input is @Nothing@.
--
-- Hint: use `err` as part of your implementation.
handleMaybe :: String -> Maybe a -> Either RuntimeError a
handleMaybe message = todo

-- | Evaluate an expression under the given environment.  This function returns
-- @Right v@ if @ρ ⊨ e ⇓ v@ according to the README.
eval :: Env -> Expr -> Either RuntimeError Value
eval ρ e = case e of
  ColorLit c -> Right . ColorV $ lift0 c
  GrayLit c -> Right . GrayV $ lift0 c
  MaskLit c -> Right . MaskV $ lift0 c
  X -> Right $ GrayV getX
  Y -> Right $ GrayV getY
  R -> Right $ GrayV . fromPolarIm $ \(Polar r _) -> r
  Theta -> Right $ GrayV . fromPolarIm $ \(Polar _ θ) -> θ
  -- The implementation of `ColorE` below.
  ColorE r g b a ->
    join $ pure mkColor <*> (eval ρ r) <*> (eval ρ g) <*> (eval ρ b) <*> (eval ρ a)
  -- Your assignment starts here, the cases above are given.  Also, see the
  -- helpers in this file, you will need to use some of them directly.
  VarE x -> todo
  Let x e1 e2 -> todo
  BinOp e1 op e2 -> todo
  UnOp op e' -> todo
  -- Hint: use liftV family of functions to implement the operations below
  Flip e -> todo
  Translate aX aY e -> todo
  Scale2 aX aY e -> todo
  Scale aK e -> todo
  Rotate θ e -> todo
  Swirl θ e -> todo
  Overlay e1 e2 -> todo
  If eGuard eTrue eFalse -> todo
  JuxtaposeH eLeft eRight -> todo
  JuxtaposeV eTop eBottom -> todo
  -- The remaining two cases are extra credit.
  Juxtapose e1 es -> todo
  Replicate n e -> todo
  where
    mkColor (GrayV r) (GrayV g) (GrayV b) (GrayV a) = Right . ColorV $ liftA4 Color r g b a
    mkColor _ _ _ _ = error "one of the color channels doesn't evaluate to a float"
    -- you can define helpers here, or anywhere in this file, as you need them

-- | Evaluate given arithmetic expression.  Use this when you need to evaluate
-- an arithmetic expression.
evalArith Pi = pi
evalArith (NumLit d) = d
evalArith (UnOpA NegateA a) = negate $ evalArith a
evalArith (UnOpA (BuiltInA f) a) = getFn f $ evalArith a
evalArith (BinOpA lhs op rhs) = evalArith lhs ⊕ evalArith rhs
  where (⊕) = case op of
              AddA -> (+)
              SubA -> (-)
              MulA -> (*)
              DivA -> (/)
evalArith _ = undefined

-- | Evaluate given binary operation over images.  Use this helper.
evalBOp (GrayV l) Add (GrayV r) = Right . GrayV $ lift2 (+) l r
evalBOp (GrayV l) Sub (GrayV r) = Right . GrayV $ lift2 (-) l r
evalBOp (GrayV l) Div (GrayV r) = Right . GrayV $ lift2 (/) l r
evalBOp (GrayV l) Mul (GrayV r) = Right . GrayV $ lift2 (*) l r
evalBOp (GrayV l) Less (GrayV r) = Right . MaskV $ lift2 (<) l r
evalBOp (GrayV l) Eq (GrayV r) = Right . MaskV $ lift2 (==) l r
evalBOp (ColorV l) Add (ColorV r) = Right . ColorV $ lift2 (+) l r
evalBOp (ColorV l) Sub (ColorV r) = Right . ColorV $ lift2 (-) l r
evalBOp (ColorV l) Mul (ColorV r) = Right . ColorV $ lift2 (*) l r
evalBOp (ColorV l) Eq (ColorV r) = Right . MaskV $ lift2 (==) l r
evalBOp (MaskV l) And (MaskV r) = Right . MaskV $ lift2 (&&) l r
evalBOp (MaskV l) Or (MaskV r) = Right . MaskV $ lift2 (||) l r
evalBOp (MaskV l) Xor (MaskV r) = Right . MaskV $ lift2 (/=) l r
evalBOp (MaskV l) Eq (MaskV r) = Right . MaskV $ lift2 (==) l r
evalBOp _ op _ = err $ "binary operator and operand mismatch. op: " ++ show op

-- | Evaluate given unary operation over images.  Use this helper.
evalUOp Negate (GrayV im) = Right . GrayV $ lift1 negate im
evalUOp (BuiltIn f) (GrayV im) = Right . GrayV $ lift1 (getFn f) im
evalUOp Negate (ColorV im) = Right . ColorV $ lift1 negate im
evalUOp (BuiltIn f) (ColorV im) = Right . ColorV $ lift1 (cMap $ getFn f) im
evalUOp Not (MaskV im) = Right . MaskV $ lift1 not im
evalUOp op _ = err $ "unary operator and operand mismatch. op: " ++ show op

-- | Run a program, this means evaluating the expression inside and rendering
-- the resulting image with given dimensions.
run :: Program -> Either RuntimeError (P.Image P.PixelRGBA8)
run program = do
  im <- eval M.empty program.image
  return $ uncurry (renderV im) program.resolution
  where
    renderV (ColorV im) = render im
    renderV (GrayV im) = render im
    renderV (MaskV im) = render im
