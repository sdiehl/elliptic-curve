module Curve.Weierstrass.SECP256K1
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

-- | SECP256K1 curve.
data SECP256K1

-- | Field of points of SECP256K1 curve.
type Fq = PrimeField 0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffefffffc2f

-- | Field of coefficients of SECP256K1 curve.
type Fr = PrimeField 0xfffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141

-- | SECP256K1 curve is a Weierstrass curve.
instance WCurve SECP256K1 Fq where
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

-- | Point of SECP256K1 curve.
type P = WPoint SECP256K1 Fq

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of SECP256K1 curve.
_a :: Fq
_a = 0x0
{-# INLINE _a #-}

-- | Coefficient @B@ of SECP256K1 curve.
_b :: Fq
_b = 0x7
{-# INLINE _b #-}

-- | Generator of SECP256K1 curve.
_g :: P
_g = A _x _y
{-# INLINE _g #-}

-- | Cofactor of SECP256K1 curve.
_h :: Integer
_h = 0x1
{-# INLINE _h #-}

-- | Characteristic of SECP256K1 curve.
_q :: Integer
_q = 0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffefffffc2f
{-# INLINE _q #-}

-- | Order of SECP256K1 curve.
_r :: Integer
_r = 0xfffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141
{-# INLINE _r #-}

-- | Coordinate @X@ of SECP256K1 curve.
_x :: Fq
_x = 0x79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798
{-# INLINE _x #-}

-- | Coordinate @Y@ of SECP256K1 curve.
_y :: Fq
_y = 0x483ada7726a3c4655da4fbfc0e1108a8fd17b448a68554199c47d08ffb10d4b8
{-# INLINE _y #-}
