{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}

module Point
  ( Point(..)
  , affineAdd
  , affineDouble
  , affineNegate
  , affineMultiply
  , affineDefined
  ) where

import Protolude

import Params (Fq, _d)

-- | Point in affine coordinates
data Point = Point Fq Fq deriving (Eq, Show, Generic, NFData)

{-# INLINE affineAdd #-}
-- | Affine addition formula
affineAdd :: Point -> Point -> Point
affineAdd p@(Point x1 y1) q@(Point x2 y2)
  | p == q    = affineDouble p
  | otherwise = Point ((x1y2 + x2y1) / (1 + dxy)) ((y1y2 + x1x2) / (1 - dxy))
  where
    x1x2 = x1 * x2
    y1y2 = y1 * y2
    x1y2 = x1 * y2
    x2y1 = x2 * y1
    dxy  = _d * x1x2 * y1y2

{-# INLINE affineDouble #-}
-- | Affine doubling formula
affineDouble :: Point -> Point
affineDouble (Point x y) = Point ((xy + xy) / (1 + dxy)) ((yy + xx) / (1 - dxy))
  where
    xx  = x * x
    xy  = x * y
    yy  = y * y
    dxy = _d * xx * yy

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

-- | Affine check well-defined
affineDefined :: Point -> Bool
affineDefined (Point x y) = -xx + yy == 1 + _d * xx * yy
  where
    xx = x * x
    yy = y * y
