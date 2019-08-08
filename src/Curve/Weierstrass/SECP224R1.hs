module Curve.Weierstrass.SECP224R1
  ( Curve(..)
  , Fq
  , Fr
  , Group(..)
  , PA
  , PJ
  , PP
  , Point(..)
  , WCurve(..)
  , WPoint
  , WACurve(..)
  , WAPoint
  , WJCurve(..)
  , WJPoint
  , WPCurve(..)
  , WPPoint
  , _a
  , _b
  , _h
  , _q
  , _r
  , _x
  , _y
  , fromAtoJ
  , fromAtoP
  , fromJtoA
  , fromPtoA
  , gA
  , gJ
  , gP
  ) where

import Protolude

import PrimeField

import Curve.Weierstrass

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
instance Curve 'Weierstrass c SECP224R1 Fq Fr => WCurve c SECP224R1 Fq Fr where
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
  x_ = const _x
  {-# INLINE x_ #-}
  y_ = const _y
  {-# INLINE y_ #-}

-- | Affine SECP224R1 curve point.
type PA = WAPoint SECP224R1 Fq Fr

-- | Affine SECP224R1 curve is a Weierstrass affine curve.
instance WACurve SECP224R1 Fq Fr where
  gA_ = gA
  {-# INLINE gA_ #-}

-- | Jacobian SECP224R1 point.
type PJ = WJPoint SECP224R1 Fq Fr

-- | Jacobian SECP224R1 curve is a Weierstrass Jacobian curve.
instance WJCurve SECP224R1 Fq Fr where
  gJ_ = gJ
  {-# INLINE gJ_ #-}

-- | Projective SECP224R1 point.
type PP = WPPoint SECP224R1 Fq Fr

-- | Projective SECP224R1 curve is a Weierstrass projective curve.
instance WPCurve SECP224R1 Fq Fr where
  gP_ = gP
  {-# INLINE gP_ #-}

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

-- | Generator of affine SECP224R1 curve.
gA :: PA
gA = A _x _y
{-# INLINE gA #-}

-- | Generator of Jacobian SECP224R1 curve.
gJ :: PJ
gJ = J _x _y 1
{-# INLINE gJ #-}

-- | Generator of projective SECP224R1 curve.
gP :: PP
gP = P _x _y 1
{-# INLINE gP #-}
