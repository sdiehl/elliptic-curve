module Curve.Weierstrass.SECP224R1
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
import Curve.Weierstrass (Point(..), WCurve(..), WPoint, WACurve(..), WAPoint)
import Group (Group(..))

-------------------------------------------------------------------------------
-- SECP224R1 curve
-------------------------------------------------------------------------------

-- | SECP224R1 curve.
data SECP224R1

-- | Field of points of SECP224R1 curve.
type Fq = PrimeField 0xffffffffffffffffffffffffffffffff000000000000000000000001

-- | Field of coefficients of SECP224R1 curve.
type Fr = PrimeField 0xffffffffffffffffffffffffffff16a2e0b8f03e13dd29455c5c2a3d

-- | SECP224R1 curve is a Weierstrass curve.
instance Curve 'Weierstrass c SECP224R1 Fq => WCurve c SECP224R1 Fq where
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

-- | Coefficient @A@ of SECP224R1 curve.
_a :: Fq
_a = 0xfffffffffffffffffffffffffffffffefffffffffffffffffffffffe
{-# INLINE _a #-}

-- | Coefficient @B@ of SECP224R1 curve.
_b :: Fq
_b = 0xb4050a850c04b3abf54132565044b0b7d7bfd8ba270b39432355ffb4
{-# INLINE _b #-}

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

-------------------------------------------------------------------------------
-- Affine coordinates
-------------------------------------------------------------------------------

-- | Affine SECP224R1 point.
type AP = WAPoint SECP224R1 Fq

-- | Affine SECP224R1 curve is a Weierstrass affine curve.
instance WACurve SECP224R1 Fq where
  gA_ = gA
  {-# INLINE gA_ #-}
  xA_ = const xA
  {-# INLINE xA_ #-}
  yA_ = const yA
  {-# INLINE yA_ #-}

-- | Generator of affine SECP224R1 curve.
gA :: AP
gA = A xA yA
{-# INLINE gA #-}

-- | Coordinate @X@ of affine SECP224R1 curve.
xA :: Fq
xA = 0xb70e0cbd6bb4bf7f321390b94a03c1d356c21122343280d6115c1d21
{-# INLINE xA #-}

-- | Coordinate @Y@ of affine SECP224R1 curve.
yA :: Fq
yA = 0xbd376388b5f723fb4c22dfe6cd4375a05a07476444d5819985007e34
{-# INLINE yA #-}
