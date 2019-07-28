module Curve.Weierstrass.BrainpoolP160T1
  ( Curve(..)
  , Fp
  , Group(..)
  , P
  , Point(..)
  , WPoint
  , WCurve(..)
  , _a
  , _b
  , _g
  , _h
  , _n
  , _p
  , _x
  , _y
  ) where

import Protolude

import PrimeField (PrimeField)

import Curve (Curve(..))
import Curve.Weierstrass (Point(..), WCurve(..), WPoint)
import Group (Group(..))

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | BrainpoolP160T1 curve.
data BrainpoolP160T1

-- | Field of BrainpoolP160T1 curve.
type Fp = PrimeField 0xe95e4a5f737059dc60dfc7ad95b3d8139515620f

-- | BrainpoolP160T1 curve is a Weierstrass curve.
instance WCurve BrainpoolP160T1 Fp where
  a_ = const _a
  {-# INLINE a_ #-}
  b_ = const _b
  {-# INLINE b_ #-}
  g_ = _g
  {-# INLINE g_ #-}
  h_ = const _h
  {-# INLINE h_ #-}
  n_ = const _n
  {-# INLINE n_ #-}
  p_ = const _p
  {-# INLINE p_ #-}
  x_ = const _x
  {-# INLINE x_ #-}
  y_ = const _y
  {-# INLINE y_ #-}

-- | Point of BrainpoolP160T1 curve.
type P = WPoint BrainpoolP160T1 Fp

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of BrainpoolP160T1 curve.
_a :: Fp
_a = 0xe95e4a5f737059dc60dfc7ad95b3d8139515620c
{-# INLINE _a #-}

-- | Coefficient @B@ of BrainpoolP160T1 curve.
_b :: Fp
_b = 0x7a556b6dae535b7b51ed2c4d7daa7a0b5c55f380
{-# INLINE _b #-}

-- | Generator of BrainpoolP160T1 curve.
_g :: P
_g = A _x _y
{-# INLINE _g #-}

-- | Cofactor of BrainpoolP160T1 curve.
_h :: Integer
_h = 0x1
{-# INLINE _h #-}

-- | Order of BrainpoolP160T1 curve.
_n :: Integer
_n = 0xe95e4a5f737059dc60df5991d45029409e60fc09
{-# INLINE _n #-}

-- | Characteristic of BrainpoolP160T1 curve.
_p :: Integer
_p = 0xe95e4a5f737059dc60dfc7ad95b3d8139515620f
{-# INLINE _p #-}

-- | Coordinate @X@ of BrainpoolP160T1 curve.
_x :: Fp
_x = 0xb199b13b9b34efc1397e64baeb05acc265ff2378
{-# INLINE _x #-}

-- | Coordinate @Y@ of BrainpoolP160T1 curve.
_y :: Fp
_y = 0xadd6718b7c7c1961f0991b842443772152c9e0ad
{-# INLINE _y #-}
