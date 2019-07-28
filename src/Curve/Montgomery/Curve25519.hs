module Curve.Montgomery.Curve25519
  ( Curve(..)
  , Fq
  , Fr
  , Group(..)
  , MPoint
  , MCurve(..)
  , P
  , Point(..)
  , _a
  , _b
  , _g
  , _h
  , _q
  , _r
  , _x
  , _y
  ) where

import Protolude

import PrimeField (PrimeField)

import Curve (Curve(..))
import Curve.Montgomery (MCurve(..), MPoint, Point(..))
import Group (Group(..))

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
instance MCurve Curve25519 Fq where
  a_ = const _a
  {-# INLINE a_ #-}
  b_ = const _b
  {-# INLINE b_ #-}
  g_ = _g
  {-# INLINE g_ #-}
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

-- | Point of Curve25519 curve.
type P = MPoint Curve25519 Fq

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

-- | Generator of Curve25519 curve.
_g :: P
_g = A _x _y
{-# INLINE _g #-}

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
