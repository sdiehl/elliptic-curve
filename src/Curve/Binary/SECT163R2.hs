module Curve.Binary.SECT163R2
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
  , _h
  , _p
  , _r
  , gA
  , xA
  , yA
  ) where

import Protolude

import BinaryField (BinaryField)
import PrimeField (PrimeField)

import Curve (Curve(..), Form(..))
import Curve.Binary (BCurve(..), BPoint, BACurve(..), BAPoint, Point(..))
import Group (Group(..))

-------------------------------------------------------------------------------
-- SECT163R2 curve
-------------------------------------------------------------------------------

-- | SECT163R2 curve.
data SECT163R2

-- | Field of points of SECT163R2 curve.
type F2m = BinaryField 0x800000000000000000000000000000000000000c9

-- | Field of coefficients of SECT163R2 curve.
type Fr = PrimeField 0x40000000000000000000292fe77e70c12a4234c33

-- | SECT163R2 curve is a binary curve.
instance Curve 'Binary c SECT163R2 F2m => BCurve c SECT163R2 F2m where
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

-- | Coefficient @A@ of SECT163R2 curve.
_a :: F2m
_a = 0x1
{-# INLINE _a #-}

-- | Coefficient @B@ of SECT163R2 curve.
_b :: F2m
_b = 0x20a601907b8c953ca1481eb10512f78744a3205fd
{-# INLINE _b #-}

-- | Cofactor of SECT163R2 curve.
_h :: Integer
_h = 0x2
{-# INLINE _h #-}

-- | Polynomial of SECT163R2 curve.
_p :: Integer
_p = 0x800000000000000000000000000000000000000c9
{-# INLINE _p #-}

-- | Order of SECT163R2 curve.
_r :: Integer
_r = 0x40000000000000000000292fe77e70c12a4234c33
{-# INLINE _r #-}

-------------------------------------------------------------------------------
-- Affine coordinates
-------------------------------------------------------------------------------

-- | Affine SECT163R2 point.
type AP = BAPoint SECT163R2 F2m

-- | Affine SECT163R2 curve is a binary affine curve.
instance BACurve SECT163R2 F2m where
  gA_ = gA
  {-# INLINE gA_ #-}
  xA_ = const xA
  {-# INLINE xA_ #-}
  yA_ = const yA
  {-# INLINE yA_ #-}

-- | Generator of affine SECT163R2 curve.
gA :: AP
gA = A xA yA
{-# INLINE gA #-}

-- | Coordinate @X@ of affine SECT163R2 curve.
xA :: F2m
xA = 0x3f0eba16286a2d57ea0991168d4994637e8343e36
{-# INLINE xA #-}

-- | Coordinate @Y@ of affine SECT163R2 curve.
yA :: F2m
yA = 0xd51fbc6c71a0094fa2cdd545b11c5c0c797324f1
{-# INLINE yA #-}
