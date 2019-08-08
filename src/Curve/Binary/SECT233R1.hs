module Curve.Binary.SECT233R1
  ( BCurve(..)
  , BPoint
  , BACurve(..)
  , BAPoint
  , BPCurve(..)
  , BPPoint
  , Curve(..)
  , F2m
  , Fr
  , Group(..)
  , PA
  , PP
  , Point(..)
  , _a
  , _b
  , _h
  , _p
  , _r
  , _x
  , _y
  , fromAtoP
  , fromPtoA
  , gA
  , gP
  ) where

import Protolude

import BinaryField
import PrimeField

import Curve.Binary

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | SECT233R1 curve.
data SECT233R1

-- | Field of points of SECT233R1 curve.
type F2m = BinaryField 0x20000000000000000000000000000000000000004000000000000000001

-- | Field of coefficients of SECT233R1 curve.
type Fr = PrimeField 0x1000000000000000000000000000013e974e72f8a6922031d2603cfe0d7

-- | SECT233R1 curve is a binary curve.
instance Curve 'Binary c SECT233R1 F2m Fr => BCurve c SECT233R1 F2m Fr where
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
  x_ = const _x
  {-# INLINE x_ #-}
  y_ = const _y
  {-# INLINE y_ #-}

-- | Affine SECT233R1 curve point.
type PA = BAPoint SECT233R1 F2m Fr

-- | Affine SECT233R1 curve is a binary affine curve.
instance BACurve SECT233R1 F2m Fr where
  gA_ = gA
  {-# INLINE gA_ #-}

-- | Projective SECT233R1 point.
type PP = BPPoint SECT233R1 F2m Fr

-- | Projective SECT233R1 curve is a binary projective curve.
instance BPCurve SECT233R1 F2m Fr where
  gP_ = gP
  {-# INLINE gP_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of SECT233R1 curve.
_a :: F2m
_a = 0x1
{-# INLINE _a #-}

-- | Coefficient @B@ of SECT233R1 curve.
_b :: F2m
_b = 0x66647ede6c332c7f8c0923bb58213b333b20e9ce4281fe115f7d8f90ad
{-# INLINE _b #-}

-- | Cofactor of SECT233R1 curve.
_h :: Integer
_h = 0x2
{-# INLINE _h #-}

-- | Polynomial of SECT233R1 curve.
_p :: Integer
_p = 0x20000000000000000000000000000000000000004000000000000000001
{-# INLINE _p #-}

-- | Order of SECT233R1 curve.
_r :: Integer
_r = 0x1000000000000000000000000000013e974e72f8a6922031d2603cfe0d7
{-# INLINE _r #-}

-- | Coordinate @X@ of SECT233R1 curve.
_x :: F2m
_x = 0xfac9dfcbac8313bb2139f1bb755fef65bc391f8b36f8f8eb7371fd558b
{-# INLINE _x #-}

-- | Coordinate @Y@ of SECT233R1 curve.
_y :: F2m
_y = 0x1006a08a41903350678e58528bebf8a0beff867a7ca36716f7e01f81052
{-# INLINE _y #-}

-- | Generator of affine SECT233R1 curve.
gA :: PA
gA = A _x _y
{-# INLINE gA #-}

-- | Generator of projective SECT233R1 curve.
gP :: PP
gP = P _x _y 1
{-# INLINE gP #-}
