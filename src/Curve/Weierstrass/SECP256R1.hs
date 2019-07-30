module Curve.Weierstrass.SECP256R1
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
-- SECP256R1 curve
-------------------------------------------------------------------------------

-- | SECP256R1 curve.
data SECP256R1

-- | Field of points of SECP256R1 curve.
type Fq = PrimeField 0xffffffff00000001000000000000000000000000ffffffffffffffffffffffff

-- | Field of coefficients of SECP256R1 curve.
type Fr = PrimeField 0xffffffff00000000ffffffffffffffffbce6faada7179e84f3b9cac2fc632551

-- | SECP256R1 curve is a Weierstrass curve.
instance Curve 'Weierstrass c SECP256R1 Fq => WCurve c SECP256R1 Fq where
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

-- | Coefficient @A@ of SECP256R1 curve.
_a :: Fq
_a = 0xffffffff00000001000000000000000000000000fffffffffffffffffffffffc
{-# INLINE _a #-}

-- | Coefficient @B@ of SECP256R1 curve.
_b :: Fq
_b = 0x5ac635d8aa3a93e7b3ebbd55769886bc651d06b0cc53b0f63bce3c3e27d2604b
{-# INLINE _b #-}

-- | Cofactor of SECP256R1 curve.
_h :: Integer
_h = 0x1
{-# INLINE _h #-}

-- | Characteristic of SECP256R1 curve.
_q :: Integer
_q = 0xffffffff00000001000000000000000000000000ffffffffffffffffffffffff
{-# INLINE _q #-}

-- | Order of SECP256R1 curve.
_r :: Integer
_r = 0xffffffff00000000ffffffffffffffffbce6faada7179e84f3b9cac2fc632551
{-# INLINE _r #-}

-------------------------------------------------------------------------------
-- Affine coordinates
-------------------------------------------------------------------------------

-- | Affine SECP256R1 point.
type AP = WAPoint SECP256R1 Fq

-- | Affine SECP256R1 curve is a Weierstrass affine curve.
instance WACurve SECP256R1 Fq where
  gA_ = gA
  {-# INLINE gA_ #-}
  xA_ = const xA
  {-# INLINE xA_ #-}
  yA_ = const yA
  {-# INLINE yA_ #-}

-- | Generator of affine SECP256R1 curve.
gA :: AP
gA = A xA yA
{-# INLINE gA #-}

-- | Coordinate @X@ of affine SECP256R1 curve.
xA :: Fq
xA = 0x6b17d1f2e12c4247f8bce6e563a440f277037d812deb33a0f4a13945d898c296
{-# INLINE xA #-}

-- | Coordinate @Y@ of affine SECP256R1 curve.
yA :: Fq
yA = 0x4fe342e2fe1a7f9b8ee7eb4a7c0f9e162bce33576b315ececbb6406837bf51f5
{-# INLINE yA #-}
