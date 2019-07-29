module Curve.Binary.SECT233K1
  ( AP
  , BCurve(..)
  , BPoint
  , BACurve(..)
  , BAPoint
  , Curve(..)
  , F2m
  , Fr
  , Group(..)
  , Point(..)
  , _a
  , _b
  , _g
  , _h
  , _p
  , _r
  , _x
  , _y
  ) where

import Protolude

import BinaryField (BinaryField)
import PrimeField (PrimeField)

import Curve (Curve(..), Form(..))
import Curve.Binary (BCurve(..), BPoint, BACurve(..), BAPoint, Point(..))
import Group (Group(..))

-------------------------------------------------------------------------------
-- SECT233K1 curve
-------------------------------------------------------------------------------

-- | SECT233K1 curve.
data SECT233K1

-- | Field of points of SECT233K1 curve.
type F2m = BinaryField 0x20000000000000000000000000000000000000004000000000000000001

-- | Field of coefficients of SECT233K1 curve.
type Fr = PrimeField 0x8000000000000000000000000000069d5bb915bcd46efb1ad5f173abdf

-- | SECT233K1 curve is a binary curve.
instance Curve 'Binary c SECT233K1 F2m => BCurve c SECT233K1 F2m where
  a_ = const _a
  {-# INLINE a_ #-}
  b_ = const _b
  {-# INLINE b_ #-}
  h_ = const _h
  {-# INLINE h_ #-}
  p_ = const _p
  {-# INLINE p_ #-}
  r_ = const _r
  {-# INLINE r_ #-}

-- | Coefficient @A@ of SECT233K1 curve.
_a :: F2m
_a = 0x0
{-# INLINE _a #-}

-- | Coefficient @B@ of SECT233K1 curve.
_b :: F2m
_b = 0x1
{-# INLINE _b #-}

-- | Cofactor of SECT233K1 curve.
_h :: Integer
_h = 0x4
{-# INLINE _h #-}

-- | Polynomial of SECT233K1 curve.
_p :: Integer
_p = 0x20000000000000000000000000000000000000004000000000000000001
{-# INLINE _p #-}

-- | Order of SECT233K1 curve.
_r :: Integer
_r = 0x8000000000000000000000000000069d5bb915bcd46efb1ad5f173abdf
{-# INLINE _r #-}

-------------------------------------------------------------------------------
-- Affine coordinates
-------------------------------------------------------------------------------

-- | Affine SECT233K1 point.
type AP = BAPoint SECT233K1 F2m

-- | Affine SECT233K1 curve is a binary affine curve.
instance BACurve SECT233K1 F2m where
  g_ = _g
  {-# INLINE g_ #-}
  x_ = const _x
  {-# INLINE x_ #-}
  y_ = const _y
  {-# INLINE y_ #-}

-- | Generator of affine SECT233K1 curve.
_g :: AP
_g = A _x _y
{-# INLINE _g #-}

-- | Coordinate @X@ of affine SECT233K1 curve.
_x :: F2m
_x = 0x17232ba853a7e731af129f22ff4149563a419c26bf50a4c9d6eefad6126
{-# INLINE _x #-}

-- | Coordinate @Y@ of affine SECT233K1 curve.
_y :: F2m
_y = 0x1db537dece819b7f70f555a67c427a8cd9bf18aeb9b56e0c11056fae6a3
{-# INLINE _y #-}
