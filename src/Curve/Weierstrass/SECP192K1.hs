module Curve.Weierstrass.SECP192K1
  ( Fp
  , P
  , _a
  , _b
  , _g
  , _h
  , _n
  , _p
  , _x
  , _y
  ) where

import Protolude

import PrimeField (PrimeField)

import Curve.Weierstrass (Point(..), WCurve(..), WPoint)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | SECP192K1 curve.
data SECP192K1

-- | Field of SECP192K1 curve.
type Fp = PrimeField 0xfffffffffffffffffffffffffffffffffffffffeffffee37

-- | SECP192K1 curve is a Weierstrass curve.
instance WCurve SECP192K1 Fp where
  a_ = const _a
  {-# INLINE a_ #-}
  b_ = const _b
  {-# INLINE b_ #-}
  g_ = _g
  {-# INLINE g_ #-}
  h_ = const _h
  {-# INLINE h_ #-}
  n_ = const _n
  {-# INLINE n_ #-}
  p_ = const _p
  {-# INLINE p_ #-}

-- | Point of SECP192K1 curve.
type P = WPoint SECP192K1 Fp

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of SECP192K1 curve.
_a :: Fp
_a = 0x0
{-# INLINE _a #-}

-- | Coefficient @B@ of SECP192K1 curve.
_b :: Fp
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

-- | Order of SECP192K1 curve.
_n :: Integer
_n = 0xfffffffffffffffffffffffe26f2fc170f69466a74defd8d
{-# INLINE _n #-}

-- | Characteristic of SECP192K1 curve.
_p :: Integer
_p = 0xfffffffffffffffffffffffffffffffffffffffeffffee37
{-# INLINE _p #-}

-- | Coordinate @X@ of SECP192K1 curve.
_x :: Fp
_x = 0xdb4ff10ec057e9ae26b07d0280b7f4341da5d1b1eae06c7d
{-# INLINE _x #-}

-- | Coordinate @Y@ of SECP192K1 curve.
_y :: Fp
_y = 0x9b2f2f6d9c5628a7844163d015be86344082aa88d95e2f9d
{-# INLINE _y #-}
