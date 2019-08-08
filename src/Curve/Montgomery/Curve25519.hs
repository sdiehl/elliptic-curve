module Curve.Montgomery.Curve25519
  ( Curve(..)
  , Fq
  , Fr
  , Group(..)
  , MCurve(..)
  , MPoint
  , MACurve(..)
  , MAPoint
  , PA
  , Point(..)
  , _a
  , _b
  , _h
  , _q
  , _r
  , _x
  , _y
  , gA
  ) where

import Protolude

import PrimeField

import Curve.Montgomery

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | Curve25519 curve.
data Curve25519

-- | Field of points of Curve25519 curve.
type Fq = PrimeField 0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffed

-- | Field of coefficients of Curve25519 curve.
type Fr = PrimeField 0x1000000000000000000000000000000014def9dea2f79cd65812631a5cf5d3ed

-- | Curve25519 curve is a Montgomery curve.
instance Curve 'Montgomery c Curve25519 Fq Fr => MCurve c Curve25519 Fq Fr where
  a_ = const _a
  {-# INLINE a_ #-}
  b_ = const _b
  {-# INLINE b_ #-}
  h_ = const _h
  {-# INLINE h_ #-}
  q_ = const _q
  {-# INLINE q_ #-}
  r_ = const _r
  {-# INLINE r_ #-}
  x_ = const _x
  {-# INLINE x_ #-}
  y_ = const _y
  {-# INLINE y_ #-}

-- | Affine Curve25519 curve point.
type PA = MAPoint Curve25519 Fq Fr

-- | Affine Curve25519 curve is a Montgomery affine curve.
instance MACurve Curve25519 Fq Fr where
  gA_ = gA
  {-# INLINE gA_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of Curve25519 curve.
_a :: Fq
_a = 0x76d06
{-# INLINE _a #-}

-- | Coefficient @B@ of Curve25519 curve.
_b :: Fq
_b = 0x1
{-# INLINE _b #-}

-- | Cofactor of Curve25519 curve.
_h :: Integer
_h = 0x8
{-# INLINE _h #-}

-- | Characteristic of Curve25519 curve.
_q :: Integer
_q = 0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffed
{-# INLINE _q #-}

-- | Order of Curve25519 curve.
_r :: Integer
_r = 0x1000000000000000000000000000000014def9dea2f79cd65812631a5cf5d3ed
{-# INLINE _r #-}

-- | Coordinate @X@ of Curve25519 curve.
_x :: Fq
_x = 0x9
{-# INLINE _x #-}

-- | Coordinate @Y@ of Curve25519 curve.
_y :: Fq
_y = 0x20ae19a1b8a086b4e01edd2c7748d14c923d4d7e6d7c61b229e9c5a27eced3d9
{-# INLINE _y #-}

-- | Generator of affine Curve25519 curve.
gA :: PA
gA = A _x _y
{-# INLINE gA #-}
