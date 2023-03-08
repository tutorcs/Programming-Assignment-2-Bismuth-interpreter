https://tutorcs.com
WeChat: cstutorcs
QQ: 749389476
Email: tutorcs@163.com
{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE DerivingVia, OverloadedRecordDot #-}
{-# LANGUAGE NamedFieldPuns, RankNTypes, TypeSynonymInstances, FlexibleInstances #-}

-- | Semantic domains.  These are the objects that the constructs in the program
-- represent, evaluate to, and/or interact with.

module Domains where

import Control.Applicative (liftA2, liftA3)
import Control.Monad
import Data.List (intersperse)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Point
import Image
import Color
import Syntax

-- | Value of an expression, it is always an image.
data Value = ColorV ImageColor
           | GrayV ImageGray
           | MaskV ImageMask

-- | lift a polymorphic image transformer to values.  This works because
-- polymorphic image transformers don't change color type.
liftVPoly :: (forall a. ImageT a) -> Value -> Value
liftVPoly f (ColorV im) = ColorV (f im)
liftVPoly f (GrayV im) = GrayV (f im)
liftVPoly f (MaskV im) = MaskV (f im)

-- | A typeclass to help lift non-generic image transformers to values.
--
-- TODO: Explain what this does to students with examples.
class ValueLike v where
  toValue :: Image v -> Value
  fromValue :: Value -> Maybe (Image v)

instance ValueLike Color where
  toValue = ColorV

  fromValue (ColorV im) = Just im
  fromValue _ = Nothing

instance ValueLike Double where
  toValue = GrayV

  fromValue (GrayV im) = Just im
  fromValue _ = Nothing

instance ValueLike Bool where
  toValue = MaskV

  fromValue (MaskV im) = Just im
  fromValue _ = Nothing

-- | Lift a binary image transformer to values.  The lifted function fails
-- (returns Nothing) if the two argument values are different types of images.
--
--
-- You can use this function to lift select or binary image operators to work on
-- values, then handle type errors.
--
-- TODO: Explain what this does to students with examples.
liftV2 :: (forall a. ValueLike a => (Image a -> Image a -> Image a)) -> Value -> Value -> Maybe Value
liftV2 f v1 v2 = case (v1, v2) of
  (ColorV im1, ColorV im2) -> Just . toValue $ f im1 im2
  (GrayV  im1, GrayV  im2) -> Just . toValue $ f im1 im2
  (MaskV  im1, MaskV  im2) -> Just . toValue $ f im1 im2
  _ -> Nothing

-- | Lift a function working on a list of images to work on a list of values.
-- The lifted function fails if the list contains images of multiple types or it is empty.
--
-- This function's implementation has large performance drawbacks (it creates a
-- lot of closures until the last element is processed).
liftVs :: (forall a. ValueLike a => [Image a] -> Image a) -> [Value] -> Maybe Value
liftVs f [] = Nothing
liftVs f (v1 : vRest) = case v1 of
  ColorV im1 -> toValue . f <$> foldl getColor (Just [im1]) vRest
  GrayV im1  -> toValue . f <$> foldl getGray (Just [im1]) vRest
  MaskV im1  -> toValue . f <$> foldl getMask (Just [im1]) vRest
  _ -> Nothing
  where
    getColor soFar (ColorV x) = (++ [x]) <$> soFar
    getColor _ _ = Nothing
    getGray soFar (GrayV x) = (++ [x]) <$> soFar
    getGray _ _ = Nothing
    getMask :: Maybe [ImageMask] -> Value -> Maybe [ImageMask]
    getMask soFar (MaskV x) = (++ [x]) <$> soFar
    getMask _ _ = Nothing

-- | Value of an arithmetic expression, it is always a number.
type ArithValue = Double

-- | Environments, these are maps from variables in scope to their values.
type Env = Map Var Value
