module Curve.Montgomery.M383
  ( Curve(..)
  , Fq
  , Fr
  , Group(..)
  , MCurve(..)
  , MPoint
  , MACurve(..)
  , MAPoint
  , PA
  , _a
  , _b
  , _h
  , _q
  , _r
  , _x
  , _y
  , gA
  , pattern A
  ) where

import Protolude

import PrimeField

import Curve.Montgomery

-------------------------------------------------------------------------------
-- Types
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
  x_ = const _x
  {-# INLINE x_ #-}
  y_ = const _y
  {-# INLINE y_ #-}

-- | Affine M383 curve point.
type PA = MAPoint M383 Fq

-- | Affine M383 curve is a Montgomery affine curve.
instance MACurve M383 Fq where
  gA_ = gA
  {-# INLINE gA_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

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

-- | Coordinate @X@ of M383 curve.
_x :: Fq
_x = 0xc
{-# INLINE _x #-}

-- | Coordinate @Y@ of M383 curve.
_y :: Fq
_y = 0x1ec7ed04aaf834af310e304b2da0f328e7c165f0e8988abd3992861290f617aa1f1b2e7d0b6e332e969991b62555e77e
{-# INLINE _y #-}

-- | Generator of affine M383 curve.
gA :: PA
gA = fromMaybe (panic "not well-defined.") (point _x _y)
{-# INLINE gA #-}
