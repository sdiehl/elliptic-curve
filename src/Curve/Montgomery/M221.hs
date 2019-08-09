module Curve.Montgomery.M221
  ( module Curve.Montgomery
  , module Curve.Montgomery.M221
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
  {-# INLINABLE a_ #-}
  b_ = const _b
  {-# INLINABLE b_ #-}
  h_ = const _h
  {-# INLINABLE h_ #-}
  q_ = const _q
  {-# INLINABLE q_ #-}
  r_ = const _r
  {-# INLINABLE r_ #-}
  x_ = const _x
  {-# INLINABLE x_ #-}
  y_ = const _y
  {-# INLINABLE y_ #-}

-- | Affine M221 curve point.
type PA = MAPoint M221 Fq Fr

-- | Affine M221 curve is a Montgomery affine curve.
instance MACurve M221 Fq Fr where
  gA_ = gA
  {-# INLINABLE gA_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of M221 curve.
_a :: Fq
_a = 0x1c93a
{-# INLINABLE _a #-}

-- | Coefficient @B@ of M221 curve.
_b :: Fq
_b = 0x1
{-# INLINABLE _b #-}

-- | Cofactor of M221 curve.
_h :: Integer
_h = 0x8
{-# INLINABLE _h #-}

-- | Characteristic of M221 curve.
_q :: Integer
_q = 0x1ffffffffffffffffffffffffffffffffffffffffffffffffffffffd
{-# INLINABLE _q #-}

-- | Order of M221 curve.
_r :: Integer
_r = 0x40000000000000000000000000015a08ed730e8a2f77f005042605b
{-# INLINABLE _r #-}

-- | Coordinate @X@ of M221 curve.
_x :: Fq
_x = 0x4
{-# INLINABLE _x #-}

-- | Coordinate @Y@ of M221 curve.
_y :: Fq
_y = 0xf7acdd2a4939571d1cef14eca37c228e61dbff10707dc6c08c5056d
{-# INLINABLE _y #-}

-- | Generator of affine M221 curve.
gA :: PA
gA = A _x _y
{-# INLINABLE gA #-}
