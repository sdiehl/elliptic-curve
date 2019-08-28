module Data.Curve.Montgomery.Curve25519
  ( module Data.Curve.Montgomery
  , module Data.Curve.Montgomery.Curve25519
  , Point(..)
  ) where

import Protolude

import Data.Field.Galois

import Data.Curve.Montgomery

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | Curve25519 curve.
data Curve25519

-- | Field of points of Curve25519 curve.
type Fq = Prime 0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffed

-- | Field of coefficients of Curve25519 curve.
type Fr = Prime 0x1000000000000000000000000000000014def9dea2f79cd65812631a5cf5d3ed

-- | Curve25519 curve is a Montgomery curve.
instance Curve 'Montgomery c Curve25519 Fq Fr => MCurve c Curve25519 Fq Fr where
  a_ = const _a
  {-# INLINABLE a_ #-}
  b_ = const _b
  {-# INLINABLE b_ #-}
  h_ = const _h
  {-# INLINABLE h_ #-}
  q_ = const _q
  {-# INLINABLE q_ #-}
  r_ = const _r
  {-# INLINABLE r_ #-}
  x_ = const _x
  {-# INLINABLE x_ #-}
  y_ = const _y
  {-# INLINABLE y_ #-}

-- | Affine Curve25519 curve point.
type PA = MAPoint Curve25519 Fq Fr

-- | Affine Curve25519 curve is a Montgomery affine curve.
instance MACurve Curve25519 Fq Fr where
  gA_ = gA
  {-# INLINABLE gA_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of Curve25519 curve.
_a :: Fq
_a = 0x76d06
{-# INLINABLE _a #-}

-- | Coefficient @B@ of Curve25519 curve.
_b :: Fq
_b = 0x1
{-# INLINABLE _b #-}

-- | Cofactor of Curve25519 curve.
_h :: Natural
_h = 0x8
{-# INLINABLE _h #-}

-- | Characteristic of Curve25519 curve.
_q :: Natural
_q = 0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffed
{-# INLINABLE _q #-}

-- | Order of Curve25519 curve.
_r :: Natural
_r = 0x1000000000000000000000000000000014def9dea2f79cd65812631a5cf5d3ed
{-# INLINABLE _r #-}

-- | Coordinate @X@ of Curve25519 curve.
_x :: Fq
_x = 0x9
{-# INLINABLE _x #-}

-- | Coordinate @Y@ of Curve25519 curve.
_y :: Fq
_y = 0x20ae19a1b8a086b4e01edd2c7748d14c923d4d7e6d7c61b229e9c5a27eced3d9
{-# INLINABLE _y #-}

-- | Generator of affine Curve25519 curve.
gA :: PA
gA = A _x _y
{-# INLINABLE gA #-}
