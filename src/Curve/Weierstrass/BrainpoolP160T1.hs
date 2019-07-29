module Curve.Weierstrass.BrainpoolP160T1
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
  , _g
  , _h
  , _q
  , _r
  , _x
  , _y
  ) where

import Protolude

import PrimeField (PrimeField)

import Curve (Curve(..), Form(..))
import Curve.Weierstrass (Point(..), WCurve(..), WPoint, WACurve(..), WAPoint)
import Group (Group(..))

-------------------------------------------------------------------------------
-- BrainpoolP160T1 curve
-------------------------------------------------------------------------------

-- | BrainpoolP160T1 curve.
data BrainpoolP160T1

-- | Field of points of BrainpoolP160T1 curve.
type Fq = PrimeField 0xe95e4a5f737059dc60dfc7ad95b3d8139515620f

-- | Field of coefficients of BrainpoolP160T1 curve.
type Fr = PrimeField 0xe95e4a5f737059dc60df5991d45029409e60fc09

-- | BrainpoolP160T1 curve is a Weierstrass curve.
instance Curve 'Weierstrass c BrainpoolP160T1 Fq => WCurve c BrainpoolP160T1 Fq where
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

-- | Coefficient @A@ of BrainpoolP160T1 curve.
_a :: Fq
_a = 0xe95e4a5f737059dc60dfc7ad95b3d8139515620c
{-# INLINE _a #-}

-- | Coefficient @B@ of BrainpoolP160T1 curve.
_b :: Fq
_b = 0x7a556b6dae535b7b51ed2c4d7daa7a0b5c55f380
{-# INLINE _b #-}

-- | Cofactor of BrainpoolP160T1 curve.
_h :: Integer
_h = 0x1
{-# INLINE _h #-}

-- | Characteristic of BrainpoolP160T1 curve.
_q :: Integer
_q = 0xe95e4a5f737059dc60dfc7ad95b3d8139515620f
{-# INLINE _q #-}

-- | Order of BrainpoolP160T1 curve.
_r :: Integer
_r = 0xe95e4a5f737059dc60df5991d45029409e60fc09
{-# INLINE _r #-}

-------------------------------------------------------------------------------
-- Affine coordinates
-------------------------------------------------------------------------------

-- | Affine BrainpoolP160T1 point.
type AP = WAPoint BrainpoolP160T1 Fq

-- | Affine BrainpoolP160T1 curve is a Weierstrass affine curve.
instance WACurve BrainpoolP160T1 Fq where
  g_ = _g
  {-# INLINE g_ #-}
  x_ = const _x
  {-# INLINE x_ #-}
  y_ = const _y
  {-# INLINE y_ #-}

-- | Generator of affine BrainpoolP160T1 curve.
_g :: AP
_g = A _x _y
{-# INLINE _g #-}

-- | Coordinate @X@ of affine BrainpoolP160T1 curve.
_x :: Fq
_x = 0xb199b13b9b34efc1397e64baeb05acc265ff2378
{-# INLINE _x #-}

-- | Coordinate @Y@ of affine BrainpoolP160T1 curve.
_y :: Fq
_y = 0xadd6718b7c7c1961f0991b842443772152c9e0ad
{-# INLINE _y #-}
