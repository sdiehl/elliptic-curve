module Curve.Weierstrass.BrainpoolP160R1
  ( Curve(..)
  , Fq
  , Fr
  , Group(..)
  , P
  , Point(..)
  , WPoint
  , WCurve(..)
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

import Curve (Curve(..))
import Curve.Weierstrass (Point(..), WCurve(..), WPoint)
import Group (Group(..))

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | BrainpoolP160R1 curve.
data BrainpoolP160R1

-- | Field of points of BrainpoolP160R1 curve.
type Fq = PrimeField 0xe95e4a5f737059dc60dfc7ad95b3d8139515620f

-- | Field of coefficients of BrainpoolP160R1 curve.
type Fr = PrimeField 0xe95e4a5f737059dc60df5991d45029409e60fc09

-- | BrainpoolP160R1 curve is a Weierstrass curve.
instance WCurve BrainpoolP160R1 Fq where
  a_ = const _a
  {-# INLINE a_ #-}
  b_ = const _b
  {-# INLINE b_ #-}
  g_ = _g
  {-# INLINE g_ #-}
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

-- | Point of BrainpoolP160R1 curve.
type P = WPoint BrainpoolP160R1 Fq

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of BrainpoolP160R1 curve.
_a :: Fq
_a = 0x340e7be2a280eb74e2be61bada745d97e8f7c300
{-# INLINE _a #-}

-- | Coefficient @B@ of BrainpoolP160R1 curve.
_b :: Fq
_b = 0x1e589a8595423412134faa2dbdec95c8d8675e58
{-# INLINE _b #-}

-- | Generator of BrainpoolP160R1 curve.
_g :: P
_g = A _x _y
{-# INLINE _g #-}

-- | Cofactor of BrainpoolP160R1 curve.
_h :: Integer
_h = 0x1
{-# INLINE _h #-}

-- | Characteristic of BrainpoolP160R1 curve.
_q :: Integer
_q = 0xe95e4a5f737059dc60dfc7ad95b3d8139515620f
{-# INLINE _q #-}

-- | Order of BrainpoolP160R1 curve.
_r :: Integer
_r = 0xe95e4a5f737059dc60df5991d45029409e60fc09
{-# INLINE _r #-}

-- | Coordinate @X@ of BrainpoolP160R1 curve.
_x :: Fq
_x = 0xbed5af16ea3f6a4f62938c4631eb5af7bdbcdbc3
{-# INLINE _x #-}

-- | Coordinate @Y@ of BrainpoolP160R1 curve.
_y :: Fq
_y = 0x1667cb477a1a8ec338f94741669c976316da6321
{-# INLINE _y #-}
