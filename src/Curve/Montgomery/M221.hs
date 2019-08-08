module Curve.Montgomery.M221
  ( Curve(..)
  , Fq
  , Fr
  , Group(..)
  , MCurve(..)
  , MPoint
  , MACurve(..)
  , MAPoint
  , PA
  , Point(..)
  , _a
  , _b
  , _h
  , _q
  , _r
  , _x
  , _y
  , gA
  ) where

import Protolude

import PrimeField

import Curve.Montgomery

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | M221 curve.
data M221

-- | Field of points of M221 curve.
type Fq = PrimeField 0x1ffffffffffffffffffffffffffffffffffffffffffffffffffffffd

-- | Field of coefficients of M221 curve.
type Fr = PrimeField 0x40000000000000000000000000015a08ed730e8a2f77f005042605b

-- | M221 curve is a Montgomery curve.
instance Curve 'Montgomery c M221 Fq Fr => MCurve c M221 Fq Fr where
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

-- | Affine M221 curve point.
type PA = MAPoint M221 Fq Fr

-- | Affine M221 curve is a Montgomery affine curve.
instance MACurve M221 Fq Fr where
  gA_ = gA
  {-# INLINE gA_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of M221 curve.
_a :: Fq
_a = 0x1c93a
{-# INLINE _a #-}

-- | Coefficient @B@ of M221 curve.
_b :: Fq
_b = 0x1
{-# INLINE _b #-}

-- | Cofactor of M221 curve.
_h :: Integer
_h = 0x8
{-# INLINE _h #-}

-- | Characteristic of M221 curve.
_q :: Integer
_q = 0x1ffffffffffffffffffffffffffffffffffffffffffffffffffffffd
{-# INLINE _q #-}

-- | Order of M221 curve.
_r :: Integer
_r = 0x40000000000000000000000000015a08ed730e8a2f77f005042605b
{-# INLINE _r #-}

-- | Coordinate @X@ of M221 curve.
_x :: Fq
_x = 0x4
{-# INLINE _x #-}

-- | Coordinate @Y@ of M221 curve.
_y :: Fq
_y = 0xf7acdd2a4939571d1cef14eca37c228e61dbff10707dc6c08c5056d
{-# INLINE _y #-}

-- | Generator of affine M221 curve.
gA :: PA
gA = A _x _y
{-# INLINE gA #-}
