module Curve.Binary.SECT239K1
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

-- | SECT239K1 curve.
data SECT239K1

-- | Field of points of SECT239K1 curve.
type F2m = BinaryField 0x800000000000000000004000000000000000000000000000000000000001

-- | Field of coefficients of SECT239K1 curve.
type Fr = PrimeField 0x2000000000000000000000000000005a79fec67cb6e91f1c1da800e478a5

-- | SECT239K1 curve is a binary curve.
instance Curve 'Binary c SECT239K1 F2m => BCurve c SECT239K1 F2m where
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

-- | Affine SECT239K1 curve point.
type PA = BAPoint SECT239K1 F2m

-- | Affine SECT239K1 curve is a binary affine curve.
instance BACurve SECT239K1 F2m where
  gA_ = gA
  {-# INLINE gA_ #-}

-- | Projective SECT239K1 point.
type PP = BPPoint SECT239K1 F2m

-- | Projective SECT239K1 curve is a binary projective curve.
instance BPCurve SECT239K1 F2m where
  gP_ = gP
  {-# INLINE gP_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of SECT239K1 curve.
_a :: F2m
_a = 0x0
{-# INLINE _a #-}

-- | Coefficient @B@ of SECT239K1 curve.
_b :: F2m
_b = 0x1
{-# INLINE _b #-}

-- | Cofactor of SECT239K1 curve.
_h :: Integer
_h = 0x4
{-# INLINE _h #-}

-- | Polynomial of SECT239K1 curve.
_p :: Integer
_p = 0x800000000000000000004000000000000000000000000000000000000001
{-# INLINE _p #-}

-- | Order of SECT239K1 curve.
_r :: Integer
_r = 0x2000000000000000000000000000005a79fec67cb6e91f1c1da800e478a5
{-# INLINE _r #-}

-- | Coordinate @X@ of SECT239K1 curve.
_x :: F2m
_x = 0x29a0b6a887a983e9730988a68727a8b2d126c44cc2cc7b2a6555193035dc
{-# INLINE _x #-}

-- | Coordinate @Y@ of SECT239K1 curve.
_y :: F2m
_y = 0x76310804f12e549bdb011c103089e73510acb275fc312a5dc6b76553f0ca
{-# INLINE _y #-}

-- | Generator of affine SECT239K1 curve.
gA :: PA
gA = A _x _y
{-# INLINE gA #-}

-- | Generator of projective SECT239K1 curve.
gP :: PP
gP = P _x _y 1
{-# INLINE gP #-}
