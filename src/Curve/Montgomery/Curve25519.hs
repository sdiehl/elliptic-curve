module Curve.Montgomery.Curve25519
  ( AP
  , Curve(..)
  , Fq
  , Fr
  , Group(..)
  , MCurve(..)
  , MPoint
  , MACurve(..)
  , MAPoint
  , Point(..)
  , _a
  , _b
  , _h
  , _q
  , _r
  , gA
  , xA
  , yA
  ) where

import Protolude

import PrimeField (PrimeField)

import Curve (Curve(..), Form(..))
import Curve.Montgomery (MCurve(..), MPoint, MACurve(..), MAPoint, Point(..))
import Group (Group(..))

-------------------------------------------------------------------------------
-- Curve25519 curve
-------------------------------------------------------------------------------

-- | Curve25519 curve.
data Curve25519

-- | Field of points of Curve25519 curve.
type Fq = PrimeField 0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffed

-- | Field of coefficients of Curve25519 curve.
type Fr = PrimeField 0x1000000000000000000000000000000014def9dea2f79cd65812631a5cf5d3ed

-- | Curve25519 curve is a Montgomery curve.
instance Curve 'Montgomery c Curve25519 Fq => MCurve c Curve25519 Fq where
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

-------------------------------------------------------------------------------
-- Affine coordinates
-------------------------------------------------------------------------------

-- | Affine Curve25519 point.
type AP = MAPoint Curve25519 Fq

-- | Affine Curve25519 curve is a Montgomery affine curve.
instance MACurve Curve25519 Fq where
  gA_ = gA
  {-# INLINE gA_ #-}
  xA_ = const xA
  {-# INLINE xA_ #-}
  yA_ = const yA
  {-# INLINE yA_ #-}

-- | Generator of affine Curve25519 curve.
gA :: AP
gA = A xA yA
{-# INLINE gA #-}

-- | Coordinate @X@ of affine Curve25519 curve.
xA :: Fq
xA = 0x9
{-# INLINE xA #-}

-- | Coordinate @Y@ of affine Curve25519 curve.
yA :: Fq
yA = 0x20ae19a1b8a086b4e01edd2c7748d14c923d4d7e6d7c61b229e9c5a27eced3d9
{-# INLINE yA #-}
