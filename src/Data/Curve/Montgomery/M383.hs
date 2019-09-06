module Data.Curve.Montgomery.M383
  ( module Data.Curve.Montgomery
  -- * M383 curve
  , module Data.Curve.Montgomery.M383
  ) where

import Protolude

import Data.Field.Galois
import GHC.Natural (Natural)

import Data.Curve.Montgomery
import Data.Curve.Montgomery.Base (MCurve(..), MACurve(..))

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | M383 curve.
data M383

-- | Field of points of M383 curve.
type Fq = Prime Q
type Q = 0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff45

-- | Field of coefficients of M383 curve.
type Fr = Prime R
type R = 0x10000000000000000000000000000000000000000000000006c79673ac36ba6e7a32576f7b1b249e46bbc225be9071d7

-- M383 curve is a Montgomery curve.
instance Curve 'Montgomery c M383 Fq Fr => MCurve c M383 Fq Fr where
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

-- | Affine M383 curve point.
type PA = MAPoint M383 Fq Fr

-- Affine M383 curve is a Montgomery affine curve.
instance MACurve M383 Fq Fr where
  gA_ = gA
  {-# INLINABLE gA_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of M383 curve.
_a :: Fq
_a = 0x1f82fe
{-# INLINABLE _a #-}

-- | Coefficient @B@ of M383 curve.
_b :: Fq
_b = 0x1
{-# INLINABLE _b #-}

-- | Cofactor of M383 curve.
_h :: Natural
_h = 0x8
{-# INLINABLE _h #-}

-- | Characteristic of M383 curve.
_q :: Natural
_q = 0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff45
{-# INLINABLE _q #-}

-- | Order of M383 curve.
_r :: Natural
_r = 0x10000000000000000000000000000000000000000000000006c79673ac36ba6e7a32576f7b1b249e46bbc225be9071d7
{-# INLINABLE _r #-}

-- | Coordinate @X@ of M383 curve.
_x :: Fq
_x = 0xc
{-# INLINABLE _x #-}

-- | Coordinate @Y@ of M383 curve.
_y :: Fq
_y = 0x1ec7ed04aaf834af310e304b2da0f328e7c165f0e8988abd3992861290f617aa1f1b2e7d0b6e332e969991b62555e77e
{-# INLINABLE _y #-}

-- | Generator of affine M383 curve.
gA :: PA
gA = A _x _y
{-# INLINABLE gA #-}
