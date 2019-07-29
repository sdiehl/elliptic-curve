module Curve.Weierstrass.BrainpoolP192T1
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
-- BrainpoolP192T1 curve
-------------------------------------------------------------------------------

-- | BrainpoolP192T1 curve.
data BrainpoolP192T1

-- | Field of points of BrainpoolP192T1 curve.
type Fq = PrimeField 0xc302f41d932a36cda7a3463093d18db78fce476de1a86297

-- | Field of coefficients of BrainpoolP192T1 curve.
type Fr = PrimeField 0xc302f41d932a36cda7a3462f9e9e916b5be8f1029ac4acc1

-- | BrainpoolP192T1 curve is a Weierstrass curve.
instance Curve 'Weierstrass c BrainpoolP192T1 Fq => WCurve c BrainpoolP192T1 Fq where
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

-- | Coefficient @A@ of BrainpoolP192T1 curve.
_a :: Fq
_a = 0xc302f41d932a36cda7a3463093d18db78fce476de1a86294
{-# INLINE _a #-}

-- | Coefficient @B@ of BrainpoolP192T1 curve.
_b :: Fq
_b = 0x13d56ffaec78681e68f9deb43b35bec2fb68542e27897b79
{-# INLINE _b #-}

-- | Cofactor of BrainpoolP192T1 curve.
_h :: Integer
_h = 0x1
{-# INLINE _h #-}

-- | Characteristic of BrainpoolP192T1 curve.
_q :: Integer
_q = 0xc302f41d932a36cda7a3463093d18db78fce476de1a86297
{-# INLINE _q #-}

-- | Order of BrainpoolP192T1 curve.
_r :: Integer
_r = 0xc302f41d932a36cda7a3462f9e9e916b5be8f1029ac4acc1
{-# INLINE _r #-}

-------------------------------------------------------------------------------
-- Affine coordinates
-------------------------------------------------------------------------------

-- | Affine BrainpoolP192T1 point.
type AP = WAPoint BrainpoolP192T1 Fq

-- | Affine BrainpoolP192T1 curve is a Weierstrass affine curve.
instance WACurve BrainpoolP192T1 Fq where
  g_ = _g
  {-# INLINE g_ #-}
  x_ = const _x
  {-# INLINE x_ #-}
  y_ = const _y
  {-# INLINE y_ #-}

-- | Generator of affine BrainpoolP192T1 curve.
_g :: AP
_g = A _x _y
{-# INLINE _g #-}

-- | Coordinate @X@ of affine BrainpoolP192T1 curve.
_x :: Fq
_x = 0x3ae9e58c82f63c30282e1fe7bbf43fa72c446af6f4618129
{-# INLINE _x #-}

-- | Coordinate @Y@ of affine BrainpoolP192T1 curve.
_y :: Fq
_y = 0x97e2c5667c2223a902ab5ca449d0084b7e5b3de7ccc01c9
{-# INLINE _y #-}
