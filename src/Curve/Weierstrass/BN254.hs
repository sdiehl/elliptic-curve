module Curve.Weierstrass.BN254
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
-- BN254 curve
-------------------------------------------------------------------------------

-- | BN254 curve.
data BN254

-- | Field of points of BN254 curve.
type Fq = PrimeField 0x30644e72e131a029b85045b68181585d97816a916871ca8d3c208c16d87cfd47

-- | Field of coefficients of BN254 curve.
type Fr = PrimeField 0x30644e72e131a029b85045b68181585d2833e84879b9709143e1f593f0000001

-- | BN254 curve is a Weierstrass curve.
instance Curve 'Weierstrass c BN254 Fq => WCurve c BN254 Fq where
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

-- | Coefficient @A@ of BN254 curve.
_a :: Fq
_a = 0x0
{-# INLINE _a #-}

-- | Coefficient @B@ of BN254 curve.
_b :: Fq
_b = 0x3
{-# INLINE _b #-}

-- | Cofactor of BN254 curve.
_h :: Integer
_h = 0x1
{-# INLINE _h #-}

-- | Characteristic of BN254 curve.
_q :: Integer
_q = 0x30644e72e131a029b85045b68181585d97816a916871ca8d3c208c16d87cfd47
{-# INLINE _q #-}

-- | Order of BN254 curve.
_r :: Integer
_r = 0x30644e72e131a029b85045b68181585d2833e84879b9709143e1f593f0000001
{-# INLINE _r #-}

-------------------------------------------------------------------------------
-- Affine coordinates
-------------------------------------------------------------------------------

-- | Affine BN254 point.
type AP = WAPoint BN254 Fq

-- | Affine BN254 curve is a Weierstrass affine curve.
instance WACurve BN254 Fq where
  gA_ = gA
  {-# INLINE gA_ #-}
  xA_ = const xA
  {-# INLINE xA_ #-}
  yA_ = const yA
  {-# INLINE yA_ #-}

-- | Generator of affine BN254 curve.
gA :: AP
gA = A xA yA
{-# INLINE gA #-}

-- | Coordinate @X@ of affine BN254 curve.
xA :: Fq
xA = 0x1
{-# INLINE xA #-}

-- | Coordinate @Y@ of affine BN254 curve.
yA :: Fq
yA = 0x2
{-# INLINE yA #-}
