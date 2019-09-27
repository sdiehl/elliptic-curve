module Data.Curve.Weierstrass.BN254C
  ( module Data.Curve.Weierstrass
  , Point(..)
  -- * BN254C curve
  , module Data.Curve.Weierstrass.BN254C
  ) where

import Protolude

import Data.Field.Galois
import GHC.Natural (Natural)

import Data.Curve.Weierstrass

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | BN254C curve.
data BN254C

-- | Field of points of BN254C curve.
type Fq = Prime Q
type Q = 0x240120db6517014efa0bab3696f8d5f06e8a555614f464babe9dbbfeeeb4a713

-- | Field of coefficients of BN254C curve.
type Fr = Prime R
type R = 0x240120db6517014efa0bab3696f8d5f00e88d43492b2cb363a75777e8d30210d

-- BN254C curve is a Weierstrass curve.
instance Curve 'Weierstrass c BN254C Fq Fr => WCurve c BN254C Fq Fr where
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

-- | Affine BN254C curve point.
type PA = WAPoint BN254C Fq Fr

-- Affine BN254C curve is a Weierstrass affine curve.
instance WACurve BN254C Fq Fr where
  gA_ = gA
  {-# INLINABLE gA_ #-}

-- | Jacobian BN254C point.
type PJ = WJPoint BN254C Fq Fr

-- Jacobian BN254C curve is a Weierstrass Jacobian curve.
instance WJCurve BN254C Fq Fr where
  gJ_ = gJ
  {-# INLINABLE gJ_ #-}

-- | Projective BN254C point.
type PP = WPPoint BN254C Fq Fr

-- Projective BN254C curve is a Weierstrass projective curve.
instance WPCurve BN254C Fq Fr where
  gP_ = gP
  {-# INLINABLE gP_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of BN254C curve.
_a :: Fq
_a = 0x0
{-# INLINABLE _a #-}

-- | Coefficient @B@ of BN254C curve.
_b :: Fq
_b = 0x2
{-# INLINABLE _b #-}

-- | Cofactor of BN254C curve.
_h :: Natural
_h = 0x1
{-# INLINABLE _h #-}

-- | Characteristic of BN254C curve.
_q :: Natural
_q = 0x240120db6517014efa0bab3696f8d5f06e8a555614f464babe9dbbfeeeb4a713
{-# INLINABLE _q #-}

-- | Order of BN254C curve.
_r :: Natural
_r = 0x240120db6517014efa0bab3696f8d5f00e88d43492b2cb363a75777e8d30210d
{-# INLINABLE _r #-}

-- | Coordinate @X@ of BN254C curve.
_x :: Fq
_x = 0x240120db6517014efa0bab3696f8d5f06e8a555614f464babe9dbbfeeeb4a712
{-# INLINABLE _x #-}

-- | Coordinate @Y@ of BN254C curve.
_y :: Fq
_y = 0x1
{-# INLINABLE _y #-}

-- | Generator of affine BN254C curve.
gA :: PA
gA = A _x _y
{-# INLINABLE gA #-}

-- | Generator of Jacobian BN254C curve.
gJ :: PJ
gJ = J _x _y 1
{-# INLINABLE gJ #-}

-- | Generator of projective BN254C curve.
gP :: PP
gP = P _x _y 1
{-# INLINABLE gP #-}
