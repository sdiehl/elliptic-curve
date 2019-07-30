module Curve.Weierstrass.BN224
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
-- BN224 curve
-------------------------------------------------------------------------------

-- | BN224 curve.
data BN224

-- | Field of points of BN224 curve.
type Fq = PrimeField 0xfffffffffff107288ec29e602c4520db42180823bb907d1287127833

-- | Field of coefficients of BN224 curve.
type Fr = PrimeField 0xfffffffffff107288ec29e602c4420db4218082b36c2accff76c58ed

-- | BN224 curve is a Weierstrass curve.
instance Curve 'Weierstrass c BN224 Fq => WCurve c BN224 Fq where
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

-- | Coefficient @A@ of BN224 curve.
_a :: Fq
_a = 0x0
{-# INLINE _a #-}

-- | Coefficient @B@ of BN224 curve.
_b :: Fq
_b = 0x3
{-# INLINE _b #-}

-- | Cofactor of BN224 curve.
_h :: Integer
_h = 0x1
{-# INLINE _h #-}

-- | Characteristic of BN224 curve.
_q :: Integer
_q = 0xfffffffffff107288ec29e602c4520db42180823bb907d1287127833
{-# INLINE _q #-}

-- | Order of BN224 curve.
_r :: Integer
_r = 0xfffffffffff107288ec29e602c4420db4218082b36c2accff76c58ed
{-# INLINE _r #-}

-------------------------------------------------------------------------------
-- Affine coordinates
-------------------------------------------------------------------------------

-- | Affine BN224 point.
type AP = WAPoint BN224 Fq

-- | Affine BN224 curve is a Weierstrass affine curve.
instance WACurve BN224 Fq where
  gA_ = gA
  {-# INLINE gA_ #-}
  xA_ = const xA
  {-# INLINE xA_ #-}
  yA_ = const yA
  {-# INLINE yA_ #-}

-- | Generator of affine BN224 curve.
gA :: AP
gA = A xA yA
{-# INLINE gA #-}

-- | Coordinate @X@ of affine BN224 curve.
xA :: Fq
xA = 0x1
{-# INLINE xA #-}

-- | Coordinate @Y@ of affine BN224 curve.
yA :: Fq
yA = 0x2
{-# INLINE yA #-}
