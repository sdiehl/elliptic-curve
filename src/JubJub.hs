module JubJub where

import Protolude

-- http://hyperelliptic.org/EFD/g1p/auto-twisted.html

_q :: Integer
_q = 0x73eda753299d7d483339d80809a1d80553bda402fffe5bfeffffffff00000001

_r :: Integer
_r = 0x0e7db4ea6533afa906673b0101343b00a6682093ccc81082d0970e5ed6f72cb7

_s :: Integer
_s = 0x0e7db4ea6533afa906673b0101343b00a6682093ccc81082d0970e5ed6f72cb7
-- 6554484396890773809930967563523245729705921265872317281365359162392183254199

_h :: Integer
_h = 8

_d :: Rational
_d = -(10240/10241)

-- | Point in affine coordinates
data PointA = PointA Rational Rational deriving Eq

-- | Affine addition formula
affineAdd :: PointA -> PointA -> PointA
affineAdd p1@(PointA x1 y1) p2@(PointA x2 y2)
  | p1 == p2  = affineDouble p1
  | otherwise = PointA x3 y3
  where
    x1x2 = x1*x2
    y1y2 = y1*y2
    x1y2 = x1*y2
    x2y1 = x2*y1
    xxyy = x1x2*y1y2
    x3   = (x1y2+x2y1)/(1+_d*xxyy)
    y3   = (y1y2+x1x2)/(1-_d*x1x2*y1y2)

-- | Affine doubling formula
affineDouble :: PointA -> PointA
affineDouble (PointA x1 y1) = PointA x3 y3
  where
    x1x1 = x1*x1
    x1y1 = x1*y1
    y1y1 = y1*y1
    xxyy = x1x1*y1y1
    x3   = (x1y1+x1y1)/(1+_d*xxyy)
    y3   = (y1y1+x1x1)/(1-_d*xxyy)

-- | Affine negation formula
affineNegate :: PointA -> PointA
affineNegate (PointA x y) = PointA (-x) y
