module Curve.Weierstrass.BrainpoolP192T1
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

import Curve (Curve(..), Group(..))
import Curve.Weierstrass (Point(..), WCurve(..), WPoint)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | BrainpoolP192T1 curve.
data BrainpoolP192T1

-- | Field of BrainpoolP192T1 curve.
type Fp = PrimeField 0xc302f41d932a36cda7a3463093d18db78fce476de1a86297

-- | BrainpoolP192T1 curve is a Weierstrass curve.
instance WCurve BrainpoolP192T1 Fp where
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

-- | Point of BrainpoolP192T1 curve.
type P = WPoint BrainpoolP192T1 Fp

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of BrainpoolP192T1 curve.
_a :: Fp
_a = 0xc302f41d932a36cda7a3463093d18db78fce476de1a86294
{-# INLINE _a #-}

-- | Coefficient @B@ of BrainpoolP192T1 curve.
_b :: Fp
_b = 0x13d56ffaec78681e68f9deb43b35bec2fb68542e27897b79
{-# INLINE _b #-}

-- | Generator of BrainpoolP192T1 curve.
_g :: P
_g = A _x _y
{-# INLINE _g #-}

-- | Cofactor of BrainpoolP192T1 curve.
_h :: Integer
_h = 0x1
{-# INLINE _h #-}

-- | Order of BrainpoolP192T1 curve.
_n :: Integer
_n = 0xc302f41d932a36cda7a3462f9e9e916b5be8f1029ac4acc1
{-# INLINE _n #-}

-- | Characteristic of BrainpoolP192T1 curve.
_p :: Integer
_p = 0xc302f41d932a36cda7a3463093d18db78fce476de1a86297
{-# INLINE _p #-}

-- | Coordinate @X@ of BrainpoolP192T1 curve.
_x :: Fp
_x = 0x3ae9e58c82f63c30282e1fe7bbf43fa72c446af6f4618129
{-# INLINE _x #-}

-- | Coordinate @Y@ of BrainpoolP192T1 curve.
_y :: Fp
_y = 0x97e2c5667c2223a902ab5ca449d0084b7e5b3de7ccc01c9
{-# INLINE _y #-}
