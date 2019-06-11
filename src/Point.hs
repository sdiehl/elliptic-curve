{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}

module Point
  ( Point(..)
  , affineAdd
  , affineDouble
  , affineNegate
  , affineMultiply
  ) where

import Protolude

import Params (_d, _r)
import Field (F)

-- | Point in affine coordinates
data Point = Point F F deriving (Eq, Show, Generic, NFData)

-- | Generator of curve
_g :: Point
_g = Point
  3465144826073652318776269530687742778270252468765361963008
  0

{-# INLINE affineAdd #-}
-- | Affine addition formula
affineAdd :: Point -> Point -> Point
affineAdd p@(Point x1 y1) q@(Point x2 y2)
  | p == q    = affineDouble p
  | otherwise = Point x3 y3
  where
    x1x2 = x1 * x2
    y1y2 = y1 * y2
    x1y2 = x1 * y2
    x2y1 = x2 * y1
    dxy  = _d * x1x2 * y1y2
    x3   = (x1y2 + x2y1) / (1 + dxy)
    y3   = (y1y2 + x1x2) / (1 - dxy)

{-# INLINE affineDouble #-}
-- | Affine doubling formula
affineDouble :: Point -> Point
affineDouble (Point x y) = Point x' y'
  where
    xx   = x * x
    xy   = x * y
    yy   = y * y
    dxy = _d * xx * yy
    x'   = (xy + xy) / (1 + dxy)
    y'   = (yy + xx) / (1 - dxy)

-- | Affine negation formula
affineNegate :: Point -> Point
affineNegate (Point x y) = Point (-x) y

{-# INLINE affineMultiply #-}
-- | Affine multiplication algorithm
affineMultiply :: Point -> Integer -> Point
affineMultiply p@(Point x y) n
  | n < 0     = affineNegate $ affineMultiply p (-n)
  | n == 0    = Point 0 1
  | even n    = p'
  | otherwise = affineAdd p p'
  where
    p' = affineMultiply (affineDouble p) (div n 2)
