module Data.Curve.Weierstrass.BN254D
  ( module Data.Curve.Weierstrass
  , Point(..)
  -- * BN254D curve
  , module Data.Curve.Weierstrass.BN254D
  ) where

import Protolude

import Data.Field.Galois
import GHC.Natural (Natural)

import Data.Curve.Weierstrass

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | BN254D curve.
data BN254D

-- | Field of points of BN254D curve.
type Fq = Prime Q
type Q = 0x24000482410f5aadb74e200f3b89d00081cf93e428f0d651e8b2dc2bb460a48b

-- | Field of coefficients of BN254D curve.
type Fr = Prime R
type R = 0x24000482410f5aadb74e200f3b89d00021cf8de127b73833d7fb71a511aa2bf5

-- BN254D curve is a Weierstrass curve.
instance Curve 'Weierstrass c BN254D Fq Fr => WCurve c BN254D Fq Fr where
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

-- | Affine BN254D curve point.
type PA = WAPoint BN254D Fq Fr

-- Affine BN254D curve is a Weierstrass affine curve.
instance WACurve BN254D Fq Fr where
  gA_ = gA
  {-# INLINABLE gA_ #-}

-- | Jacobian BN254D point.
type PJ = WJPoint BN254D Fq Fr

-- Jacobian BN254D curve is a Weierstrass Jacobian curve.
instance WJCurve BN254D Fq Fr where
  gJ_ = gJ
  {-# INLINABLE gJ_ #-}

-- | Projective BN254D point.
type PP = WPPoint BN254D Fq Fr

-- Projective BN254D curve is a Weierstrass projective curve.
instance WPCurve BN254D Fq Fr where
  gP_ = gP
  {-# INLINABLE gP_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of BN254D curve.
_a :: Fq
_a = 0x0
{-# INLINABLE _a #-}

-- | Coefficient @B@ of BN254D curve.
_b :: Fq
_b = 0x2
{-# INLINABLE _b #-}

-- | Cofactor of BN254D curve.
_h :: Natural
_h = 0x1
{-# INLINABLE _h #-}

-- | Characteristic of BN254D curve.
_q :: Natural
_q = 0x24000482410f5aadb74e200f3b89d00081cf93e428f0d651e8b2dc2bb460a48b
{-# INLINABLE _q #-}

-- | Order of BN254D curve.
_r :: Natural
_r = 0x24000482410f5aadb74e200f3b89d00021cf8de127b73833d7fb71a511aa2bf5
{-# INLINABLE _r #-}

-- | Coordinate @X@ of BN254D curve.
_x :: Fq
_x = 0x24000482410f5aadb74e200f3b89d00081cf93e428f0d651e8b2dc2bb460a48a
{-# INLINABLE _x #-}

-- | Coordinate @Y@ of BN254D curve.
_y :: Fq
_y = 0x1
{-# INLINABLE _y #-}

-- | Generator of affine BN254D curve.
gA :: PA
gA = A _x _y
{-# INLINABLE gA #-}

-- | Generator of Jacobian BN254D curve.
gJ :: PJ
gJ = J _x _y 1
{-# INLINABLE gJ #-}

-- | Generator of projective BN254D curve.
gP :: PP
gP = P _x _y 1
{-# INLINABLE gP #-}
