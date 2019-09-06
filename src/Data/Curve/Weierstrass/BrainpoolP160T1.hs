module Data.Curve.Weierstrass.BrainpoolP160T1
  ( module Data.Curve.Weierstrass
  -- * BrainpoolP160T1 curve
  , module Data.Curve.Weierstrass.BrainpoolP160T1
  ) where

import Protolude

import Data.Field.Galois
import GHC.Natural (Natural)

import Data.Curve.Weierstrass
import Data.Curve.Weierstrass.Base (WCurve(..), WACurve(..), WJCurve(..), WPCurve(..))

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | BrainpoolP160T1 curve.
data BrainpoolP160T1

-- | Field of points of BrainpoolP160T1 curve.
type Fq = Prime Q
type Q = 0xe95e4a5f737059dc60dfc7ad95b3d8139515620f

-- | Field of coefficients of BrainpoolP160T1 curve.
type Fr = Prime R
type R = 0xe95e4a5f737059dc60df5991d45029409e60fc09

-- BrainpoolP160T1 curve is a Weierstrass curve.
instance Curve 'Weierstrass c BrainpoolP160T1 Fq Fr => WCurve c BrainpoolP160T1 Fq Fr where
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

-- | Affine BrainpoolP160T1 curve point.
type PA = WAPoint BrainpoolP160T1 Fq Fr

-- Affine BrainpoolP160T1 curve is a Weierstrass affine curve.
instance WACurve BrainpoolP160T1 Fq Fr where
  gA_ = gA
  {-# INLINABLE gA_ #-}

-- | Jacobian BrainpoolP160T1 point.
type PJ = WJPoint BrainpoolP160T1 Fq Fr

-- Jacobian BrainpoolP160T1 curve is a Weierstrass Jacobian curve.
instance WJCurve BrainpoolP160T1 Fq Fr where
  gJ_ = gJ
  {-# INLINABLE gJ_ #-}

-- | Projective BrainpoolP160T1 point.
type PP = WPPoint BrainpoolP160T1 Fq Fr

-- Projective BrainpoolP160T1 curve is a Weierstrass projective curve.
instance WPCurve BrainpoolP160T1 Fq Fr where
  gP_ = gP
  {-# INLINABLE gP_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of BrainpoolP160T1 curve.
_a :: Fq
_a = 0xe95e4a5f737059dc60dfc7ad95b3d8139515620c
{-# INLINABLE _a #-}

-- | Coefficient @B@ of BrainpoolP160T1 curve.
_b :: Fq
_b = 0x7a556b6dae535b7b51ed2c4d7daa7a0b5c55f380
{-# INLINABLE _b #-}

-- | Cofactor of BrainpoolP160T1 curve.
_h :: Natural
_h = 0x1
{-# INLINABLE _h #-}

-- | Characteristic of BrainpoolP160T1 curve.
_q :: Natural
_q = 0xe95e4a5f737059dc60dfc7ad95b3d8139515620f
{-# INLINABLE _q #-}

-- | Order of BrainpoolP160T1 curve.
_r :: Natural
_r = 0xe95e4a5f737059dc60df5991d45029409e60fc09
{-# INLINABLE _r #-}

-- | Coordinate @X@ of BrainpoolP160T1 curve.
_x :: Fq
_x = 0xb199b13b9b34efc1397e64baeb05acc265ff2378
{-# INLINABLE _x #-}

-- | Coordinate @Y@ of BrainpoolP160T1 curve.
_y :: Fq
_y = 0xadd6718b7c7c1961f0991b842443772152c9e0ad
{-# INLINABLE _y #-}

-- | Generator of affine BrainpoolP160T1 curve.
gA :: PA
gA = A _x _y
{-# INLINABLE gA #-}

-- | Generator of Jacobian BrainpoolP160T1 curve.
gJ :: PJ
gJ = J _x _y 1
{-# INLINABLE gJ #-}

-- | Generator of projective BrainpoolP160T1 curve.
gP :: PP
gP = P _x _y 1
{-# INLINABLE gP #-}
