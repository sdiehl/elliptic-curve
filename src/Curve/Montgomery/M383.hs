module Curve.Montgomery.M383
  ( AP
  , Curve(..)
  , Fq
  , Fr
  , Group(..)
  , MCurve(..)
  , MPoint
  , MACurve(..)
  , MAPoint
  , Point(..)
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
import Curve.Montgomery (MCurve(..), MPoint, MACurve(..), MAPoint, Point(..))
import Group (Group(..))

-------------------------------------------------------------------------------
-- M383 curve
-------------------------------------------------------------------------------

-- | M383 curve.
data M383

-- | Field of points of M383 curve.
type Fq = PrimeField 0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff45

-- | Field of coefficients of M383 curve.
type Fr = PrimeField 0x10000000000000000000000000000000000000000000000006c79673ac36ba6e7a32576f7b1b249e46bbc225be9071d7

-- | M383 curve is a Montgomery curve.
instance Curve 'Montgomery c M383 Fq => MCurve c M383 Fq where
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

-- | Coefficient @A@ of M383 curve.
_a :: Fq
_a = 0x1f82fe
{-# INLINE _a #-}

-- | Coefficient @B@ of M383 curve.
_b :: Fq
_b = 0x1
{-# INLINE _b #-}

-- | Cofactor of M383 curve.
_h :: Integer
_h = 0x8
{-# INLINE _h #-}

-- | Characteristic of M383 curve.
_q :: Integer
_q = 0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff45
{-# INLINE _q #-}

-- | Order of M383 curve.
_r :: Integer
_r = 0x10000000000000000000000000000000000000000000000006c79673ac36ba6e7a32576f7b1b249e46bbc225be9071d7
{-# INLINE _r #-}

-------------------------------------------------------------------------------
-- Affine coordinates
-------------------------------------------------------------------------------

-- | Affine M383 point.
type AP = MAPoint M383 Fq

-- | Affine M383 curve is a Montgomery affine curve.
instance MACurve M383 Fq where
  gA_ = gA
  {-# INLINE gA_ #-}
  xA_ = const xA
  {-# INLINE xA_ #-}
  yA_ = const yA
  {-# INLINE yA_ #-}

-- | Generator of affine M383 curve.
gA :: AP
gA = A xA yA
{-# INLINE gA #-}

-- | Coordinate @X@ of affine M383 curve.
xA :: Fq
xA = 0xc
{-# INLINE xA #-}

-- | Coordinate @Y@ of affine M383 curve.
yA :: Fq
yA = 0x1ec7ed04aaf834af310e304b2da0f328e7c165f0e8988abd3992861290f617aa1f1b2e7d0b6e332e969991b62555e77e
{-# INLINE yA #-}
