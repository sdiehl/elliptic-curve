module Curve.Weierstrass.BN256
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

-- | BN256 curve.
data BN256

-- | Field of points of BN256 curve.
type Fq = PrimeField 0xfffffffffffcf0cd46e5f25eee71a49f0cdc65fb12980a82d3292ddbaed33013

-- | Field of coefficients of BN256 curve.
type Fr = PrimeField 0xfffffffffffcf0cd46e5f25eee71a49e0cdc65fb1299921af62d536cd10b500d

-- | BN256 curve is a Weierstrass curve.
instance WCurve BN256 Fq where
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

-- | Point of BN256 curve.
type P = WPoint BN256 Fq

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of BN256 curve.
_a :: Fq
_a = 0x0
{-# INLINE _a #-}

-- | Coefficient @B@ of BN256 curve.
_b :: Fq
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

-- | Characteristic of BN256 curve.
_q :: Integer
_q = 0xfffffffffffcf0cd46e5f25eee71a49f0cdc65fb12980a82d3292ddbaed33013
{-# INLINE _q #-}

-- | Order of BN256 curve.
_r :: Integer
_r = 0xfffffffffffcf0cd46e5f25eee71a49e0cdc65fb1299921af62d536cd10b500d
{-# INLINE _r #-}

-- | Coordinate @X@ of BN256 curve.
_x :: Fq
_x = 0x1
{-# INLINE _x #-}

-- | Coordinate @Y@ of BN256 curve.
_y :: Fq
_y = 0x2
{-# INLINE _y #-}
