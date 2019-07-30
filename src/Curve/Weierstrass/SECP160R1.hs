module Curve.Weierstrass.SECP160R1
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
-- SECP160R1 curve
-------------------------------------------------------------------------------

-- | SECP160R1 curve.
data SECP160R1

-- | Field of points of SECP160R1 curve.
type Fq = PrimeField 0xffffffffffffffffffffffffffffffff7fffffff

-- | Field of coefficients of SECP160R1 curve.
type Fr = PrimeField 0x100000000000000000001f4c8f927aed3ca752257

-- | SECP160R1 curve is a Weierstrass curve.
instance Curve 'Weierstrass c SECP160R1 Fq => WCurve c SECP160R1 Fq where
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

-- | Coefficient @A@ of SECP160R1 curve.
_a :: Fq
_a = 0xffffffffffffffffffffffffffffffff7ffffffc
{-# INLINE _a #-}

-- | Coefficient @B@ of SECP160R1 curve.
_b :: Fq
_b = 0x1c97befc54bd7a8b65acf89f81d4d4adc565fa45
{-# INLINE _b #-}

-- | Cofactor of SECP160R1 curve.
_h :: Integer
_h = 0x1
{-# INLINE _h #-}

-- | Characteristic of SECP160R1 curve.
_q :: Integer
_q = 0xffffffffffffffffffffffffffffffff7fffffff
{-# INLINE _q #-}

-- | Order of SECP160R1 curve.
_r :: Integer
_r = 0x100000000000000000001f4c8f927aed3ca752257
{-# INLINE _r #-}

-------------------------------------------------------------------------------
-- Affine coordinates
-------------------------------------------------------------------------------

-- | Affine SECP160R1 point.
type AP = WAPoint SECP160R1 Fq

-- | Affine SECP160R1 curve is a Weierstrass affine curve.
instance WACurve SECP160R1 Fq where
  gA_ = gA
  {-# INLINE gA_ #-}
  xA_ = const xA
  {-# INLINE xA_ #-}
  yA_ = const yA
  {-# INLINE yA_ #-}

-- | Generator of affine SECP160R1 curve.
gA :: AP
gA = A xA yA
{-# INLINE gA #-}

-- | Coordinate @X@ of affine SECP160R1 curve.
xA :: Fq
xA = 0x4a96b5688ef573284664698968c38bb913cbfc82
{-# INLINE xA #-}

-- | Coordinate @Y@ of affine SECP160R1 curve.
yA :: Fq
yA = 0x23a628553168947d59dcc912042351377ac5fb32
{-# INLINE yA #-}
