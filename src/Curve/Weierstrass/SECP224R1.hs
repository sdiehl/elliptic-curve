module Curve.Weierstrass.SECP224R1
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

-- | SECP224R1 curve.
data SECP224R1

-- | Field of points of SECP224R1 curve.
type Fq = PrimeField 0xffffffffffffffffffffffffffffffff000000000000000000000001

-- | Field of coefficients of SECP224R1 curve.
type Fr = PrimeField 0xffffffffffffffffffffffffffff16a2e0b8f03e13dd29455c5c2a3d

-- | SECP224R1 curve is a Weierstrass curve.
instance WCurve SECP224R1 Fq where
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

-- | Point of SECP224R1 curve.
type P = WPoint SECP224R1 Fq

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of SECP224R1 curve.
_a :: Fq
_a = 0xfffffffffffffffffffffffffffffffefffffffffffffffffffffffe
{-# INLINE _a #-}

-- | Coefficient @B@ of SECP224R1 curve.
_b :: Fq
_b = 0xb4050a850c04b3abf54132565044b0b7d7bfd8ba270b39432355ffb4
{-# INLINE _b #-}

-- | Generator of SECP224R1 curve.
_g :: P
_g = A _x _y
{-# INLINE _g #-}

-- | Cofactor of SECP224R1 curve.
_h :: Integer
_h = 0x1
{-# INLINE _h #-}

-- | Characteristic of SECP224R1 curve.
_q :: Integer
_q = 0xffffffffffffffffffffffffffffffff000000000000000000000001
{-# INLINE _q #-}

-- | Order of SECP224R1 curve.
_r :: Integer
_r = 0xffffffffffffffffffffffffffff16a2e0b8f03e13dd29455c5c2a3d
{-# INLINE _r #-}

-- | Coordinate @X@ of SECP224R1 curve.
_x :: Fq
_x = 0xb70e0cbd6bb4bf7f321390b94a03c1d356c21122343280d6115c1d21
{-# INLINE _x #-}

-- | Coordinate @Y@ of SECP224R1 curve.
_y :: Fq
_y = 0xbd376388b5f723fb4c22dfe6cd4375a05a07476444d5819985007e34
{-# INLINE _y #-}
