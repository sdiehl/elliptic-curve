module Curve.Weierstrass.SECP112R1
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
-- SECP112R1 curve
-------------------------------------------------------------------------------

-- | SECP112R1 curve.
data SECP112R1

-- | Field of points of SECP112R1 curve.
type Fq = PrimeField 0xdb7c2abf62e35e668076bead208b

-- | Field of coefficients of SECP112R1 curve.
type Fr = PrimeField 0xdb7c2abf62e35e7628dfac6561c5

-- | SECP112R1 curve is a Weierstrass curve.
instance Curve 'Weierstrass c SECP112R1 Fq => WCurve c SECP112R1 Fq where
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

-- | Coefficient @A@ of SECP112R1 curve.
_a :: Fq
_a = 0xdb7c2abf62e35e668076bead2088
{-# INLINE _a #-}

-- | Coefficient @B@ of SECP112R1 curve.
_b :: Fq
_b = 0x659ef8ba043916eede8911702b22
{-# INLINE _b #-}

-- | Cofactor of SECP112R1 curve.
_h :: Integer
_h = 0x1
{-# INLINE _h #-}

-- | Characteristic of SECP112R1 curve.
_q :: Integer
_q = 0xdb7c2abf62e35e668076bead208b
{-# INLINE _q #-}

-- | Order of SECP112R1 curve.
_r :: Integer
_r = 0xdb7c2abf62e35e7628dfac6561c5
{-# INLINE _r #-}

-------------------------------------------------------------------------------
-- Affine coordinates
-------------------------------------------------------------------------------

-- | Affine SECP112R1 point.
type AP = WAPoint SECP112R1 Fq

-- | Affine SECP112R1 curve is a Weierstrass affine curve.
instance WACurve SECP112R1 Fq where
  gA_ = gA
  {-# INLINE gA_ #-}
  xA_ = const xA
  {-# INLINE xA_ #-}
  yA_ = const yA
  {-# INLINE yA_ #-}

-- | Generator of affine SECP112R1 curve.
gA :: AP
gA = A xA yA
{-# INLINE gA #-}

-- | Coordinate @X@ of affine SECP112R1 curve.
xA :: Fq
xA = 0x9487239995a5ee76b55f9c2f098
{-# INLINE xA #-}

-- | Coordinate @Y@ of affine SECP112R1 curve.
yA :: Fq
yA = 0xa89ce5af8724c0a23e0e0ff77500
{-# INLINE yA #-}
