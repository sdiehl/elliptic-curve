module Curve.Weierstrass.BN256
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

-- | BN256 curve.
data BN256

-- | Field of BN256 curve.
type Fp = PrimeField 0xfffffffffffcf0cd46e5f25eee71a49f0cdc65fb12980a82d3292ddbaed33013

-- | BN256 curve is a Weierstrass curve.
instance WCurve BN256 Fp where
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

-- | Point of BN256 curve.
type P = WPoint BN256 Fp

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of BN256 curve.
_a :: Fp
_a = 0x0
{-# INLINE _a #-}

-- | Coefficient @B@ of BN256 curve.
_b :: Fp
_b = 0x3
{-# INLINE _b #-}

-- | Generator of BN256 curve.
_g :: P
_g = A _x _y
{-# INLINE _g #-}

-- | Cofactor of BN256 curve.
_h :: Integer
_h = 0x1
{-# INLINE _h #-}

-- | Order of BN256 curve.
_n :: Integer
_n = 0xfffffffffffcf0cd46e5f25eee71a49e0cdc65fb1299921af62d536cd10b500d
{-# INLINE _n #-}

-- | Characteristic of BN256 curve.
_p :: Integer
_p = 0xfffffffffffcf0cd46e5f25eee71a49f0cdc65fb12980a82d3292ddbaed33013
{-# INLINE _p #-}

-- | Coordinate @X@ of BN256 curve.
_x :: Fp
_x = 0x1
{-# INLINE _x #-}

-- | Coordinate @Y@ of BN256 curve.
_y :: Fp
_y = 0x2
{-# INLINE _y #-}
