module Curve.Binary.SECT283R1
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

-- | SECT283R1 curve.
data SECT283R1

-- | Field of points of SECT283R1 curve.
type F2m = BinaryField 0x800000000000000000000000000000000000000000000000000000000000000000010a1

-- | Field of coefficients of SECT283R1 curve.
type Fr = PrimeField 0x3ffffffffffffffffffffffffffffffffffef90399660fc938a90165b042a7cefadb307

-- | SECT283R1 curve is a binary curve.
instance Curve 'Binary c SECT283R1 F2m => BCurve c SECT283R1 F2m where
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

-- | Affine SECT283R1 curve point.
type PA = BAPoint SECT283R1 F2m

-- | Affine SECT283R1 curve is a binary affine curve.
instance BACurve SECT283R1 F2m where
  gA_ = gA
  {-# INLINE gA_ #-}

-- | Projective SECT283R1 point.
type PP = BPPoint SECT283R1 F2m

-- | Projective SECT283R1 curve is a binary projective curve.
instance BPCurve SECT283R1 F2m where
  gP_ = gP
  {-# INLINE gP_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of SECT283R1 curve.
_a :: F2m
_a = 0x1
{-# INLINE _a #-}

-- | Coefficient @B@ of SECT283R1 curve.
_b :: F2m
_b = 0x27b680ac8b8596da5a4af8a19a0303fca97fd7645309fa2a581485af6263e313b79a2f5
{-# INLINE _b #-}

-- | Cofactor of SECT283R1 curve.
_h :: Integer
_h = 0x2
{-# INLINE _h #-}

-- | Polynomial of SECT283R1 curve.
_p :: Integer
_p = 0x800000000000000000000000000000000000000000000000000000000000000000010a1
{-# INLINE _p #-}

-- | Order of SECT283R1 curve.
_r :: Integer
_r = 0x3ffffffffffffffffffffffffffffffffffef90399660fc938a90165b042a7cefadb307
{-# INLINE _r #-}

-- | Coordinate @X@ of SECT283R1 curve.
_x :: F2m
_x = 0x5f939258db7dd90e1934f8c70b0dfec2eed25b8557eac9c80e2e198f8cdbecd86b12053
{-# INLINE _x #-}

-- | Coordinate @Y@ of SECT283R1 curve.
_y :: F2m
_y = 0x3676854fe24141cb98fe6d4b20d02b4516ff702350eddb0826779c813f0df45be8112f4
{-# INLINE _y #-}

-- | Generator of affine SECT283R1 curve.
gA :: PA
gA = A _x _y
{-# INLINE gA #-}

-- | Generator of projective SECT283R1 curve.
gP :: PP
gP = P _x _y 1
{-# INLINE gP #-}
