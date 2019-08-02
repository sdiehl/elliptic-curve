module Curve.Binary.SECT283K1
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
  , _a
  , _b
  , _h
  , _p
  , _r
  , _x
  , _y
  , gA
  , gP
  , pattern A
  , pattern P
  ) where

import Protolude

import BinaryField
import PrimeField

import Curve.Binary

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | SECT283K1 curve.
data SECT283K1

-- | Field of points of SECT283K1 curve.
type F2m = BinaryField 0x800000000000000000000000000000000000000000000000000000000000000000010a1

-- | Field of coefficients of SECT283K1 curve.
type Fr = PrimeField 0x1ffffffffffffffffffffffffffffffffffe9ae2ed07577265dff7f94451e061e163c61

-- | SECT283K1 curve is a binary curve.
instance Curve 'Binary c SECT283K1 F2m => BCurve c SECT283K1 F2m where
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

-- | Affine SECT283K1 curve point.
type PA = BAPoint SECT283K1 F2m

-- | Affine SECT283K1 curve is a binary affine curve.
instance BACurve SECT283K1 F2m where
  gA_ = gA
  {-# INLINE gA_ #-}

-- | Projective SECT283K1 point.
type PP = BPPoint SECT283K1 F2m

-- | Projective SECT283K1 curve is a binary projective curve.
instance BPCurve SECT283K1 F2m where
  gP_ = gP
  {-# INLINE gP_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of SECT283K1 curve.
_a :: F2m
_a = 0x0
{-# INLINE _a #-}

-- | Coefficient @B@ of SECT283K1 curve.
_b :: F2m
_b = 0x1
{-# INLINE _b #-}

-- | Cofactor of SECT283K1 curve.
_h :: Integer
_h = 0x4
{-# INLINE _h #-}

-- | Polynomial of SECT283K1 curve.
_p :: Integer
_p = 0x800000000000000000000000000000000000000000000000000000000000000000010a1
{-# INLINE _p #-}

-- | Order of SECT283K1 curve.
_r :: Integer
_r = 0x1ffffffffffffffffffffffffffffffffffe9ae2ed07577265dff7f94451e061e163c61
{-# INLINE _r #-}

-- | Coordinate @X@ of SECT283K1 curve.
_x :: F2m
_x = 0x503213f78ca44883f1a3b8162f188e553cd265f23c1567a16876913b0c2ac2458492836
{-# INLINE _x #-}

-- | Coordinate @Y@ of SECT283K1 curve.
_y :: F2m
_y = 0x1ccda380f1c9e318d90f95d07e5426fe87e45c0e8184698e45962364e34116177dd2259
{-# INLINE _y #-}

-- | Generator of affine SECT283K1 curve.
gA :: PA
gA = fromMaybe (panic "not well-defined.") (point _x _y)
{-# INLINE gA #-}

-- | Generator of projective SECT283K1 curve.
gP :: PP
gP = fromMaybe (panic "not well-defined.") (point _x _y)
{-# INLINE gP #-}
