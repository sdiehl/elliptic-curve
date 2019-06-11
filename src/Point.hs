{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}

module Point
  ( PointA(..)
  , affineAdd
  , affineDouble
  , affineNegate
  , affineMultiply
  ) where

import Protolude

import Pairing.Fq (Fq)

import Params (_d)

-- | Point in affine coordinates
data PointA = PointA Fq Fq deriving (Eq, Show, Generic, NFData)

{-# INLINE affineAdd #-}
-- | Affine addition formula
affineAdd :: PointA -> PointA -> PointA
affineAdd p@(PointA x1 y1) q@(PointA x2 y2)
  | p == q    = affineDouble p
  | otherwise = PointA x3 y3
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
affineDouble :: PointA -> PointA
affineDouble (PointA x y) = PointA x' y'
  where
    xx   = x * x
    xy   = x * y
    yy   = y * y
    dxy = _d * xx * yy
    x'   = (xy + xy) / (1 + dxy)
    y'   = (yy + xx) / (1 - dxy)

-- | Affine negation formula
affineNegate :: PointA -> PointA
affineNegate (PointA x y) = PointA (-x) y

{-# INLINE affineMultiply #-}
-- | Affine multiplication algorithm
affineMultiply :: Int -> PointA -> PointA
affineMultiply n p@(PointA x y)
  | n == 0    = PointA 0 1
  | n < 0     = affineNegate $ affineMultiply (-n) p
  | even n    = p'
  | otherwise = affineAdd p p'
  where
    p' = affineMultiply (div n 2) (affineDouble p)
