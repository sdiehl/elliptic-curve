module Data.Curve.Montgomery.M221
  ( module Data.Curve.Montgomery
  , Point(..)
  -- * M221 curve
  , module Data.Curve.Montgomery.M221
  ) where

import Protolude

import Data.Field.Galois
import GHC.Natural (Natural)

import Data.Curve.Montgomery

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | M221 curve.
data M221

-- | Field of points of M221 curve.
type Fq = Prime Q
type Q = 0x1ffffffffffffffffffffffffffffffffffffffffffffffffffffffd

-- | Field of coefficients of M221 curve.
type Fr = Prime R
type R = 0x40000000000000000000000000015a08ed730e8a2f77f005042605b

-- M221 curve is a Montgomery curve.
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

-- | Affine M221 curve point.
type PA = MAPoint M221 Fq Fr

-- Affine M221 curve is a Montgomery affine curve.
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
_h :: Natural
_h = 0x8
{-# INLINABLE _h #-}

-- | Characteristic of M221 curve.
_q :: Natural
_q = 0x1ffffffffffffffffffffffffffffffffffffffffffffffffffffffd
{-# INLINABLE _q #-}

-- | Order of M221 curve.
_r :: Natural
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
