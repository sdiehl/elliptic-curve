module Curve.Weierstrass.SECP224K1
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

-- | SECP224K1 curve.
data SECP224K1

-- | Field of points of SECP224K1 curve.
type Fq = PrimeField 0xfffffffffffffffffffffffffffffffffffffffffffffffeffffe56d

-- | Field of coefficients of SECP224K1 curve.
type Fr = PrimeField 0x10000000000000000000000000001dce8d2ec6184caf0a971769fb1f7

-- | SECP224K1 curve is a Weierstrass curve.
instance WCurve SECP224K1 Fq where
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

-- | Point of SECP224K1 curve.
type P = WPoint SECP224K1 Fq

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of SECP224K1 curve.
_a :: Fq
_a = 0x0
{-# INLINE _a #-}

-- | Coefficient @B@ of SECP224K1 curve.
_b :: Fq
_b = 0x5
{-# INLINE _b #-}

-- | Generator of SECP224K1 curve.
_g :: P
_g = A _x _y
{-# INLINE _g #-}

-- | Cofactor of SECP224K1 curve.
_h :: Integer
_h = 0x1
{-# INLINE _h #-}

-- | Characteristic of SECP224K1 curve.
_q :: Integer
_q = 0xfffffffffffffffffffffffffffffffffffffffffffffffeffffe56d
{-# INLINE _q #-}

-- | Order of SECP224K1 curve.
_r :: Integer
_r = 0x10000000000000000000000000001dce8d2ec6184caf0a971769fb1f7
{-# INLINE _r #-}

-- | Coordinate @X@ of SECP224K1 curve.
_x :: Fq
_x = 0xa1455b334df099df30fc28a169a467e9e47075a90f7e650eb6b7a45c
{-# INLINE _x #-}

-- | Coordinate @Y@ of SECP224K1 curve.
_y :: Fq
_y = 0x7e089fed7fba344282cafbd6f7e319f7c0b0bd59e2ca4bdb556d61a5
{-# INLINE _y #-}
