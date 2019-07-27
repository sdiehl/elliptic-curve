module Curve.Weierstrass.SECP256R1
  ( Curve(..)
  , Fp
  , Group(..)
  , P
  , Point(..)
  , WPoint
  , WCurve(..)
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

import Curve (Curve(..), Group(..))
import Curve.Weierstrass (Point(..), WCurve(..), WPoint)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | SECP256R1 curve.
data SECP256R1

-- | Field of SECP256R1 curve.
type Fp = PrimeField 0xffffffff00000001000000000000000000000000ffffffffffffffffffffffff

-- | SECP256R1 curve is a Weierstrass curve.
instance WCurve SECP256R1 Fp where
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
  x_ = const _x
  {-# INLINE x_ #-}
  y_ = const _y
  {-# INLINE y_ #-}

-- | Point of SECP256R1 curve.
type P = WPoint SECP256R1 Fp

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of SECP256R1 curve.
_a :: Fp
_a = 0xffffffff00000001000000000000000000000000fffffffffffffffffffffffc
{-# INLINE _a #-}

-- | Coefficient @B@ of SECP256R1 curve.
_b :: Fp
_b = 0x5ac635d8aa3a93e7b3ebbd55769886bc651d06b0cc53b0f63bce3c3e27d2604b
{-# INLINE _b #-}

-- | Generator of SECP256R1 curve.
_g :: P
_g = A _x _y
{-# INLINE _g #-}

-- | Cofactor of SECP256R1 curve.
_h :: Integer
_h = 0x1
{-# INLINE _h #-}

-- | Order of SECP256R1 curve.
_n :: Integer
_n = 0xffffffff00000000ffffffffffffffffbce6faada7179e84f3b9cac2fc632551
{-# INLINE _n #-}

-- | Characteristic of SECP256R1 curve.
_p :: Integer
_p = 0xffffffff00000001000000000000000000000000ffffffffffffffffffffffff
{-# INLINE _p #-}

-- | Coordinate @X@ of SECP256R1 curve.
_x :: Fp
_x = 0x6b17d1f2e12c4247f8bce6e563a440f277037d812deb33a0f4a13945d898c296
{-# INLINE _x #-}

-- | Coordinate @Y@ of SECP256R1 curve.
_y :: Fp
_y = 0x4fe342e2fe1a7f9b8ee7eb4a7c0f9e162bce33576b315ececbb6406837bf51f5
{-# INLINE _y #-}
