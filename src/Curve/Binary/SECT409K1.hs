module Curve.Binary.SECT409K1
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
  ) where

import Protolude

import BinaryField
import PrimeField

import Curve.Binary

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | SECT409K1 curve.
data SECT409K1

-- | Field of points of SECT409K1 curve.
type F2m = BinaryField 0x2000000000000000000000000000000000000000000000000000000000000000000000000000000008000000000000000000001

-- | Field of coefficients of SECT409K1 curve.
type Fr = PrimeField 0x7ffffffffffffffffffffffffffffffffffffffffffffffffffe5f83b2d4ea20400ec4557d5ed3e3e7ca5b4b5c83b8e01e5fcf

-- | SECT409K1 curve is a binary curve.
instance Curve 'Binary c SECT409K1 F2m => BCurve c SECT409K1 F2m where
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

-- | Affine SECT409K1 curve point.
type PA = BAPoint SECT409K1 F2m

-- | Affine SECT409K1 curve is a binary affine curve.
instance BACurve SECT409K1 F2m where
  gA_ = gA
  {-# INLINE gA_ #-}

-- | Projective SECT409K1 point.
type PP = BPPoint SECT409K1 F2m

-- | Projective SECT409K1 curve is a binary projective curve.
instance BPCurve SECT409K1 F2m where
  gP_ = gP
  {-# INLINE gP_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of SECT409K1 curve.
_a :: F2m
_a = 0x0
{-# INLINE _a #-}

-- | Coefficient @B@ of SECT409K1 curve.
_b :: F2m
_b = 0x1
{-# INLINE _b #-}

-- | Cofactor of SECT409K1 curve.
_h :: Integer
_h = 0x4
{-# INLINE _h #-}

-- | Polynomial of SECT409K1 curve.
_p :: Integer
_p = 0x2000000000000000000000000000000000000000000000000000000000000000000000000000000008000000000000000000001
{-# INLINE _p #-}

-- | Order of SECT409K1 curve.
_r :: Integer
_r = 0x7ffffffffffffffffffffffffffffffffffffffffffffffffffe5f83b2d4ea20400ec4557d5ed3e3e7ca5b4b5c83b8e01e5fcf
{-# INLINE _r #-}

-- | Coordinate @X@ of SECT409K1 curve.
_x :: F2m
_x = 0x60f05f658f49c1ad3ab1890f7184210efd0987e307c84c27accfb8f9f67cc2c460189eb5aaaa62ee222eb1b35540cfe9023746
{-# INLINE _x #-}

-- | Coordinate @Y@ of SECT409K1 curve.
_y :: F2m
_y = 0x1e369050b7c4e42acba1dacbf04299c3460782f918ea427e6325165e9ea10e3da5f6c42e9c55215aa9ca27a5863ec48d8e0286b
{-# INLINE _y #-}

-- | Generator of affine SECT409K1 curve.
gA :: PA
gA = fromMaybe (panic "not well-defined.") (point _x _y)
{-# INLINE gA #-}

-- | Generator of projective SECT409K1 curve.
gP :: PP
gP = fromMaybe (panic "not well-defined.") (point _x _y)
{-# INLINE gP #-}
