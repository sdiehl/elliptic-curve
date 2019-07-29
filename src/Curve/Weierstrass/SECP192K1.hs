module Curve.Weierstrass.SECP192K1
  ( AP
  , Curve(..)
  , Fq
  , Fr
  , Group(..)
  , Point(..)
  , WCurve(..)
  , WPoint
  , WACurve(..)
  , WAPoint
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

import Curve (Curve(..), Form(..))
import Curve.Weierstrass (Point(..), WCurve(..), WPoint, WACurve(..), WAPoint)
import Group (Group(..))

-------------------------------------------------------------------------------
-- SECP192K1 curve
-------------------------------------------------------------------------------

-- | SECP192K1 curve.
data SECP192K1

-- | Field of points of SECP192K1 curve.
type Fq = PrimeField 0xfffffffffffffffffffffffffffffffffffffffeffffee37

-- | Field of coefficients of SECP192K1 curve.
type Fr = PrimeField 0xfffffffffffffffffffffffe26f2fc170f69466a74defd8d

-- | SECP192K1 curve is a Weierstrass curve.
instance Curve 'Weierstrass c SECP192K1 Fq => WCurve c SECP192K1 Fq where
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

-- | Coefficient @A@ of SECP192K1 curve.
_a :: Fq
_a = 0x0
{-# INLINE _a #-}

-- | Coefficient @B@ of SECP192K1 curve.
_b :: Fq
_b = 0x3
{-# INLINE _b #-}

-- | Cofactor of SECP192K1 curve.
_h :: Integer
_h = 0x1
{-# INLINE _h #-}

-- | Characteristic of SECP192K1 curve.
_q :: Integer
_q = 0xfffffffffffffffffffffffffffffffffffffffeffffee37
{-# INLINE _q #-}

-- | Order of SECP192K1 curve.
_r :: Integer
_r = 0xfffffffffffffffffffffffe26f2fc170f69466a74defd8d
{-# INLINE _r #-}

-------------------------------------------------------------------------------
-- Affine coordinates
-------------------------------------------------------------------------------

-- | Affine SECP192K1 point.
type AP = WAPoint SECP192K1 Fq

-- | Affine SECP192K1 curve is a Weierstrass affine curve.
instance WACurve SECP192K1 Fq where
  g_ = _g
  {-# INLINE g_ #-}
  x_ = const _x
  {-# INLINE x_ #-}
  y_ = const _y
  {-# INLINE y_ #-}

-- | Generator of affine SECP192K1 curve.
_g :: AP
_g = A _x _y
{-# INLINE _g #-}

-- | Coordinate @X@ of affine SECP192K1 curve.
_x :: Fq
_x = 0xdb4ff10ec057e9ae26b07d0280b7f4341da5d1b1eae06c7d
{-# INLINE _x #-}

-- | Coordinate @Y@ of affine SECP192K1 curve.
_y :: Fq
_y = 0x9b2f2f6d9c5628a7844163d015be86344082aa88d95e2f9d
{-# INLINE _y #-}
