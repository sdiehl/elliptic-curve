module Curve.Weierstrass.SECP256K1
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
-- SECP256K1 curve
-------------------------------------------------------------------------------

-- | SECP256K1 curve.
data SECP256K1

-- | Field of points of SECP256K1 curve.
type Fq = PrimeField 0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffefffffc2f

-- | Field of coefficients of SECP256K1 curve.
type Fr = PrimeField 0xfffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141

-- | SECP256K1 curve is a Weierstrass curve.
instance Curve 'Weierstrass c SECP256K1 Fq => WCurve c SECP256K1 Fq where
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

-- | Coefficient @A@ of SECP256K1 curve.
_a :: Fq
_a = 0x0
{-# INLINE _a #-}

-- | Coefficient @B@ of SECP256K1 curve.
_b :: Fq
_b = 0x7
{-# INLINE _b #-}

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

-------------------------------------------------------------------------------
-- Affine coordinates
-------------------------------------------------------------------------------

-- | Affine SECP256K1 point.
type AP = WAPoint SECP256K1 Fq

-- | Affine SECP256K1 curve is a Weierstrass affine curve.
instance WACurve SECP256K1 Fq where
  gA_ = gA
  {-# INLINE gA_ #-}
  xA_ = const xA
  {-# INLINE xA_ #-}
  yA_ = const yA
  {-# INLINE yA_ #-}

-- | Generator of affine SECP256K1 curve.
gA :: AP
gA = A xA yA
{-# INLINE gA #-}

-- | Coordinate @X@ of affine SECP256K1 curve.
xA :: Fq
xA = 0x79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798
{-# INLINE xA #-}

-- | Coordinate @Y@ of affine SECP256K1 curve.
yA :: Fq
yA = 0x483ada7726a3c4655da4fbfc0e1108a8fd17b448a68554199c47d08ffb10d4b8
{-# INLINE yA #-}
