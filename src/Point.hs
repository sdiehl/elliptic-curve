{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}

module Point (PointA(..), affineAdd, affineDouble, affineNegate) where

import Protolude

import Field (F(..))

-- | Coefficient d in -x^2+y^2=1+dx^2y^2
_d :: F
_d = -(10240 / 10241)

-- | Point in affine coordinates
data PointA = PointA F F deriving (Eq, Show, Generic)

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
    xxyy = x1x2 * y1y2
    x3   = (x1y2 + x2y1) / (1 + _d * xxyy)
    y3   = (y1y2 + x1x2) / (1 - _d * x1x2 * y1y2)

-- | Affine doubling formula
affineDouble :: PointA -> PointA
affineDouble (PointA x y) = PointA x' y'
  where
    xx   = x * x
    xy   = x * y
    yy   = y * y
    xxyy = xx * yy
    x'   = (xy + xy) / (1 + _d * xxyy)
    y'   = (yy + xx) / (1 - _d * xxyy)

-- | Affine negation formula
affineNegate :: PointA -> PointA
affineNegate (PointA x y) = PointA (-x) y
