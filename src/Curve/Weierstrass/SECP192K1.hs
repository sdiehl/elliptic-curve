module Curve.Weierstrass.SECP192K1
  ( Curve(..)
  , Fq
  , Fr
  , Group(..)
  , P
  , Point(..)
  , WPoint
  , WCurve(..)
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
import Curve.Weierstrass (Point(..), WCurve(..), WPoint)
import Group (Group(..))

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | SECP192K1 curve.
data SECP192K1

-- | Field of points of SECP192K1 curve.
type Fq = PrimeField 0xfffffffffffffffffffffffffffffffffffffffeffffee37

-- | Field of coefficients of SECP192K1 curve.
type Fr = PrimeField 0xfffffffffffffffffffffffe26f2fc170f69466a74defd8d

-- | SECP192K1 curve is a Weierstrass curve.
instance WCurve SECP192K1 Fq where
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

-- | Point of SECP192K1 curve.
type P = WPoint SECP192K1 Fq

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of SECP192K1 curve.
_a :: Fq
_a = 0x0
{-# INLINE _a #-}

-- | Coefficient @B@ of SECP192K1 curve.
_b :: Fq
_b = 0x3
{-# INLINE _b #-}

-- | Generator of SECP192K1 curve.
_g :: P
_g = A _x _y
{-# INLINE _g #-}

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

-- | Coordinate @X@ of SECP192K1 curve.
_x :: Fq
_x = 0xdb4ff10ec057e9ae26b07d0280b7f4341da5d1b1eae06c7d
{-# INLINE _x #-}

-- | Coordinate @Y@ of SECP192K1 curve.
_y :: Fq
_y = 0x9b2f2f6d9c5628a7844163d015be86344082aa88d95e2f9d
{-# INLINE _y #-}
